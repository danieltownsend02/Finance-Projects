# Load required libraries
library(quantmod)
library(TTR)
library(rugarch)
library(PerformanceAnalytics)

# Get data for AAPL and SPY
getSymbols(c("AAPL", "SPY"), from = "2010-01-01")

# Set parameters for Moving Averages and commission
short_ma <- 20
long_ma <- 50
commission <- 0.001

# Prices and returns
price <- Cl(AAPL)
returns <- na.omit(diff(log(price)))
benchmark_ret <- na.omit(diff(log(Cl(SPY))))

# Calculate the SMAs (20 and 50 periods)
sma_short <- SMA(price, short_ma)
sma_long <- SMA(price, long_ma)

# SMA 20 Crossover Signal (for AAPL)
sma20_AAPL_ts <- Lag(ifelse(Lag(Cl(AAPL)) < Lag(sma_short) & Cl(AAPL) > sma_short, 1,
                            ifelse(Lag(Cl(AAPL)) > Lag(sma_short) & Cl(AAPL) < sma_short, -1, 0)))
sma20_AAPL_ts[is.na(sma20_AAPL_ts)] <- 0

# SMA 50 Crossover Signal (for AAPL)
sma50_AAPL_ts <- Lag(ifelse(Lag(Cl(AAPL)) < Lag(sma_long) & Cl(AAPL) > sma_long, 1,
                            ifelse(Lag(Cl(AAPL)) > Lag(sma_long) & Cl(AAPL) < sma_long, -1, 0)))
sma50_AAPL_ts[is.na(sma50_AAPL_ts)] <- 0

# SMA 20 and SMA 50 Crossover Signal (for AAPL)
sma_AAPL_ts <- Lag(ifelse(Lag(sma_short) < Lag(sma_long) & sma_short > sma_long, 1,
                          ifelse(Lag(sma_short) > Lag(sma_long) & sma_short < sma_long, -1, 0)))
sma_AAPL_ts[is.na(sma_AAPL_ts)] <- 0

# Create a Position Strategy (1 for Buy, 0 for Sell)
sma_AAPL_strat <- ifelse(sma_AAPL_ts > 1, 0, 1)  # Initialize with no position
for (i in 1:length(Cl(AAPL))) {
  sma_AAPL_strat[i] <- ifelse(sma_AAPL_ts[i] == 1, 1, ifelse(sma_AAPL_ts[i] == -1, 0, sma_AAPL_strat[i - 1]))
}

# Ensure that NA positions are filled with 1 (to keep the strategy open initially)
sma_AAPL_strat[is.na(sma_AAPL_strat)] <- 1

# Apply the GARCH model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

# Fit the GARCH model to returns
fit <- ugarchfit(spec = spec, data = returns)
vol <- sigma(fit)  # Get volatility forecast from GARCH model

# Fixed 75th percentile volatility filter
vol_thresh <- quantile(vol, 0.75, na.rm = TRUE)
vol_filter <- ifelse(vol < vol_thresh, 1, 0)
vol_filter <- xts(vol_filter, order.by = index(vol))

# Apply volatility filter to position
position <- sma_AAPL_strat[index(vol_filter)] * vol_filter

# Strategy returns: position * returns
strategy_ret <- position * returns[index(position)]
trades <- ifelse(Lag(position) != position, commission, 0)
strategy_net <- strategy_ret - trades
strategy_net[is.na(strategy_net)] <- 0

# Compare strategy with benchmark (SPY)
benchmark_net <- benchmark_ret[index(strategy_net)]
comparison <- na.omit(merge(strategy_net, benchmark_net))
colnames(comparison) <- c("MA+GARCH Strategy", "SPY Benchmark")

# Performance evaluation
charts.PerformanceSummary(comparison, main = "MA+GARCH Strategy vs SPY")
chart.Drawdown(comparison, main = "Drawdowns: MA+GARCH vs SPY")
table.AnnualizedReturns(comparison)
SortinoRatio(comparison, MAR = 0)
maxDrawdown(comparison[,1])



# Visual inspection of MA crossover with price
barChart(AAPL['2024'], theme = 'white', main = "AAPL 2024 Price with SMAs")
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'red')
legend('bottomright', inset = 0.02, legend = c('AAPL', 'MA20', 'MA50'),
       col = c('black', 'blue', 'red'), lty = c(1, 1, 1), cex = 0.7)

# Plot estimated volatility from GARCH model
plot(fit@fit$sigma, type = 'l',
     main = "Estimated Volatility for Net Strategy",
     ylab = "Volatility", xlab = "Date",
     col = "blue", lwd = 2)
grid()

# Generate and plot GARCH forecast
forecast <- ugarchforecast(fit, n.ahead = 5)
plot(forecast, which = 1)  # Conditional mean forecast
plot(forecast, which = 3)  # Volatility forecast

# Plot closing prices of AAPL 
plot(AAPL$AAPL.Close, type = "l", col = "skyblue", 
     main = "Closing Prices of AAPL and GOOG",
     ylab = "Price", xlab = "Date")
legend("topleft", legend = c("AAPL"),
       col = c("skyblue"), lty = 1, cex = 0.8)

