#MA strat + GARCH
# Load required libraries
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(rugarch)

# Load data
getSymbols(c("AAPL", "GOOG", "AMZN", "SPY"), from = "2010-01-01")

# Set commission
commission <- 0.001

# Benchmark return
benchmark_ret <- diff(log(Cl(SPY)))

### ======================
### AAPL Strategy + Plots
### ======================
aapl_price <- Cl(AAPL)
aapl_returns <- na.omit(diff(log(aapl_price)))

sma20 <- SMA(aapl_price, 20)
sma50 <- SMA(aapl_price, 50)

# Plot price and SMAs
barChart(AAPL['2024'], theme = 'white')
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'red')
legend('bottomright', inset = 0.02, legend = c('AAPL', 'MA20', 'MA50'),
       col = c('black', 'blue', 'red'), lty = c(1,1,1), cex = 0.7)

signal <- Lag(ifelse(Lag(sma20) < Lag(sma50) & sma20 > sma50, 1,
                     ifelse(Lag(sma20) > Lag(sma50) & sma20 < sma50, -1, 0)))
signal[is.na(signal)] <- 0

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
position <- xts(position, order.by = index(aapl_price))

# GARCH filter
spec <- ugarchspec(variance.model = list(model = "sGARCH"),
                   mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                   distribution.model = "std")
fit <- ugarchfit(spec = spec, data = aapl_returns)
vol <- sigma(fit)
vol_thresh <- quantile(vol, 0.75, na.rm = TRUE)
vol_filter <- ifelse(vol < vol_thresh, 1, 0)
vol_filter <- xts(vol_filter, order.by = index(vol))
position <- position[index(vol_filter)] * vol_filter

# Net returns
strategy_ret <- position * aapl_returns[index(position)]
trades <- ifelse(Lag(position) != position, commission, 0)
strategy_net <- strategy_ret - trades
strategy_net[is.na(strategy_net)] <- 0

# Compare to benchmark
bench <- benchmark_ret[index(strategy_net)]
comparison_aapl <- na.omit(merge(strategy_net, bench))
colnames(comparison_aapl) <- c("AAPL MA+GARCH", "SPY Benchmark")

### ======================
### GOOG Strategy + Plots
### ======================
GOOG_price <- Cl(GOOG)
GOOG_returns <- na.omit(diff(log(GOOG_price)))

sma20 <- SMA(GOOG_price, 20)
sma50 <- SMA(GOOG_price, 50)

barChart(GOOG['2024'], theme = 'white')
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'red')
legend('bottomright', inset = 0.02, legend = c('GOOG', 'MA20', 'MA50'),
       col = c('black', 'blue', 'red'), lty = c(1,1,1), cex = 0.7)

signal <- Lag(ifelse(Lag(sma20) < Lag(sma50) & sma20 > sma50, 1,
                     ifelse(Lag(sma20) > Lag(sma50) & sma20 < sma50, -1, 0)))
signal[is.na(signal)] <- 0

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
position <- xts(position, order.by = index(GOOG_price))

fit <- ugarchfit(spec = spec, data = GOOG_returns)
vol <- sigma(fit)
vol_thresh <- quantile(vol, 0.75, na.rm = TRUE)
vol_filter <- ifelse(vol < vol_thresh, 1, 0)
vol_filter <- xts(vol_filter, order.by = index(vol))
position <- position[index(vol_filter)] * vol_filter

strategy_ret <- position * GOOG_returns[index(position)]
trades <- ifelse(Lag(position) != position, commission, 0)
strategy_net <- strategy_ret - trades
strategy_net[is.na(strategy_net)] <- 0

bench <- benchmark_ret[index(strategy_net)]
comparison_GOOG <- na.omit(merge(strategy_net, bench))
colnames(comparison_GOOG) <- c("GOOG MA+GARCH", "SPY Benchmark")

### ======================
### AMZN Strategy + Plots
### ======================
amzn_price <- Cl(AMZN)
amzn_returns <- na.omit(diff(log(amzn_price)))

sma20 <- SMA(amzn_price, 20)
sma50 <- SMA(amzn_price, 50)

barChart(AMZN['2024'], theme = 'white')
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'red')
legend('bottomright', inset = 0.02, legend = c('AMZN', 'MA20', 'MA50'),
       col = c('black', 'blue', 'red'), lty = c(1,1,1), cex = 0.7)

signal <- Lag(ifelse(Lag(sma20) < Lag(sma50) & sma20 > sma50, 1,
                     ifelse(Lag(sma20) > Lag(sma50) & sma20 < sma50, -1, 0)))
signal[is.na(signal)] <- 0

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
position <- xts(position, order.by = index(amzn_price))

fit <- ugarchfit(spec = spec, data = amzn_returns)
vol <- sigma(fit)
vol_thresh <- quantile(vol, 0.75, na.rm = TRUE)
vol_filter <- ifelse(vol < vol_thresh, 1, 0)
vol_filter <- xts(vol_filter, order.by = index(vol))
position <- position[index(vol_filter)] * vol_filter

strategy_ret <- position * amzn_returns[index(position)]
trades <- ifelse(Lag(position) != position, commission, 0)
strategy_net <- strategy_ret - trades
strategy_net[is.na(strategy_net)] <- 0

bench <- benchmark_ret[index(strategy_net)]
comparison_amzn <- na.omit(merge(strategy_net, bench))
colnames(comparison_amzn) <- c("AMZN MA+GARCH", "SPY Benchmark")

### ======================
### Final Performance Charts
### ======================

charts.PerformanceSummary(comparison_aapl, main = "AAPL Strategy vs SPY")
charts.PerformanceSummary(comparison_GOOG, main = "GOOG Strategy vs SPY")
charts.PerformanceSummary(comparison_amzn, main = "AMZN Strategy vs SPY")

print(table.AnnualizedReturns(comparison_aapl))
print(table.AnnualizedReturns(comparison_GOOG))
print(table.AnnualizedReturns(comparison_amzn))
