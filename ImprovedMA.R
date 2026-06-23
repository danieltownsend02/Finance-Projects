
# Load libraries
library(quantmod)
library(TTR)
library(rugarch)
library(PerformanceAnalytics)

# Get data
getSymbols(c("AAPL", "SPY"), from = "2010-01-01")

# Set parameters
short_ma <- 20
long_ma <- 50
commission <- 0.001

# Prices and returns
price <- Cl(AAPL)
returns <- na.omit(diff(log(price)))
benchmark_ret <- na.omit(diff(log(Cl(SPY))))

# Moving averages
sma_short <- SMA(price, short_ma)
sma_long <- SMA(price, long_ma)

# Visual inspection of MA crossover
barChart(AAPL['2024'], theme = 'white', main = "AAPL 2024 Price with SMAs")
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'red')
legend('bottomright', inset = 0.02, legend = c('AAPL', 'MA20', 'MA50'),
       col = c('black', 'blue', 'red'), lty = c(1,1,1), cex = 0.7)

# Signal logic
signal <- Lag(ifelse(Lag(sma_short) < Lag(sma_long) & sma_short > sma_long, 1,
                     ifelse(Lag(sma_short) > Lag(sma_long) & sma_short < sma_long, -1, 0)))
signal[is.na(signal)] <- 0

# Position logic
position <- rep(NA, length(signal))
position[1] <- 0
for (i in 2:length(signal)) {
  if (signal[i] == 1) {
    position[i] <- 1
  } else if (signal[i] == -1) {
    position[i] <- 0
  } else {
    position[i] <- position[i - 1]
  }
}
position <- xts(position, order.by = index(price))

# Fit GARCH to returns
spec <- ugarchspec(
  variance.model = list(model = "sGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)
fit <- ugarchfit(spec = spec, data = returns)
vol <- sigma(fit)

# Fixed 75th percentile volatility filter
vol_thresh <- quantile(vol, 0.75, na.rm = TRUE)
vol_filter <- ifelse(vol < vol_thresh, 1, 0)
vol_filter <- xts(vol_filter, order.by = index(vol))

# Apply volatility filter to position
position <- position[index(vol_filter)] * vol_filter

# Strategy returns
strategy_ret <- position * returns[index(position)]
trades <- ifelse(Lag(position) != position, commission, 0)
strategy_net <- strategy_ret - trades
strategy_net[is.na(strategy_net)] <- 0

# Compare to benchmark
benchmark_net <- benchmark_ret[index(strategy_net)]
comparison <- na.omit(merge(strategy_net, benchmark_net))
colnames(comparison) <- c("MA+GARCH Strategy", "SPY Benchmark")

# Performance evaluation
charts.PerformanceSummary(comparison, main = "MA+GARCH Strategy vs SPY")
chart.Drawdown(comparison, main = "Drawdowns: MA+GARCH vs SPY")
print(table.AnnualizedReturns(comparison))

# Sortino ratio (downside-risk adjusted)
print(SortinoRatio(comparison[,1], MAR = 0))

