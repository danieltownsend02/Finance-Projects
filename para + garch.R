# Parabolic + Garch filter
# Required libraries
library(quantmod)
library(TTR)
library(rugarch)
library(PerformanceAnalytics)

# Download historical price data
getSymbols(c("AAPL", "GOOG", "AMZN", "SPY"), from = "2010-01-01")

# log Benchmark Return (SPY)
benchmark_price <- Cl(SPY)
benchmark_ret <- na.omit(diff(log(benchmark_price)))
commission <- 0.001

# AAPL STRATEGY
# extracts clsoing prices and daily log returns
aapl_price <- Cl(AAPL)
aapl_returns <- na.omit(diff(log(aapl_price)))


# SAR Signal
sar_aapl <- SAR(HLC(AAPL), accel = c(0.02, 0.2))
signal_aapl <- Lag(ifelse(Lag(aapl_price) < Lag(sar_aapl) & aapl_price > sar_aapl, 1,
                          ifelse(Lag(aapl_price) > Lag(sar_aapl) & aapl_price < sar_aapl, -1, 0)))
#buy when prices crosses above SAR, sell when crosses below. lag used to avoid look ahead bias
signal_aapl[is.na(signal_aapl)] <- 0
# fills NA with 0
# 1 for long, 0 for out of market
position_aapl <- rep(NA, length(signal_aapl))
position_aapl[1] <- 0
for (i in 2:length(signal_aapl)) {
  if (signal_aapl[i] == 1) {
    position_aapl[i] <- 1
  } else if (signal_aapl[i] == -1) {
    position_aapl[i] <- 0
  } else {
    position_aapl[i] <- position_aapl[i - 1]
  }
}
#convert to xts for compatibility
position_aapl <- xts(position_aapl, order.by = index(aapl_price))

# GARCH Volatility Filter
#basic GARCH (1,1) model. only trades when volatility is below 75th percentile
spec_aapl <- ugarchspec(variance.model = list(model = "sGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "std")
fit_aapl <- ugarchfit(spec = spec_aapl, data = aapl_returns)
vol_aapl <- sigma(fit_aapl)
vol_thresh_aapl <- quantile(vol_aapl, 0.75, na.rm = TRUE)
vol_filter_aapl <- ifelse(vol_aapl < vol_thresh_aapl, 1, 0)
vol_filter_aapl <- xts(vol_filter_aapl, order.by = index(vol_aapl))

# Filter Position
#filters to skip days when volaitlity is too high
#avoids overfitting and high risk trades minimising bias
position_aapl <- position_aapl[index(vol_filter_aapl)] * vol_filter_aapl

# Strategy Returns
#subtracts commission 10 bps
strategy_ret_aapl <- position_aapl * aapl_returns[index(position_aapl)]
trades_aapl <- ifelse(Lag(position_aapl) != position_aapl, commission, 0)
strategy_net_aapl <- strategy_ret_aapl - trades_aapl
strategy_net_aapl[is.na(strategy_net_aapl)] <- 0

# Performance Comparison
bench_aapl <- benchmark_ret[index(strategy_net_aapl)]
comparison_aapl <- na.omit(merge(strategy_net_aapl, bench_aapl))
colnames(comparison_aapl) <- c("AAPL SAR+GARCH", "SPY Benchmark")

#plotting Chart
par(mfrow = c(1, 2))
barChart(AAPL['2024'], theme = 'white', main = "AAPL Price + SAR")
addSAR(accel = c(0.02, 0.2))
charts.PerformanceSummary(comparison_aapl, main = "AAPL SAR+GARCH vs SPY")

# =========================
# GOOG STRATEGY
# =========================
GOOG_price <- Cl(GOOG)
GOOG_returns <- na.omit(diff(log(GOOG_price)))

sar_GOOG <- SAR(HLC(GOOG), accel = c(0.02, 0.2))
signal_GOOG <- Lag(ifelse(Lag(GOOG_price) < Lag(sar_GOOG) & GOOG_price > sar_GOOG, 1,
                          ifelse(Lag(GOOG_price) > Lag(sar_GOOG) & GOOG_price < sar_GOOG, -1, 0)))
signal_GOOG[is.na(signal_GOOG)] <- 0

position_GOOG <- rep(NA, length(signal_GOOG))
position_GOOG[1] <- 0
for (i in 2:length(signal_GOOG)) {
  if (signal_GOOG[i] == 1) {
    position_GOOG[i] <- 1
  } else if (signal_GOOG[i] == -1) {
    position_GOOG[i] <- 0
  } else {
    position_GOOG[i] <- position_GOOG[i - 1]
  }
}
position_GOOG <- xts(position_GOOG, order.by = index(GOOG_price))

spec_GOOG <- ugarchspec(variance.model = list(model = "sGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "std")
fit_GOOG <- ugarchfit(spec = spec_GOOG, data = GOOG_returns)
vol_GOOG <- sigma(fit_GOOG)
vol_thresh_GOOG <- quantile(vol_GOOG, 0.75, na.rm = TRUE)
vol_filter_GOOG <- ifelse(vol_GOOG < vol_thresh_GOOG, 1, 0)
vol_filter_GOOG <- xts(vol_filter_GOOG, order.by = index(vol_GOOG))

position_GOOG <- position_GOOG[index(vol_filter_GOOG)] * vol_filter_GOOG
strategy_ret_GOOG <- position_GOOG * GOOG_returns[index(position_GOOG)]
trades_GOOG <- ifelse(Lag(position_GOOG) != position_GOOG, commission, 0)
strategy_net_GOOG <- strategy_ret_GOOG - trades_GOOG
strategy_net_GOOG[is.na(strategy_net_GOOG)] <- 0

bench_GOOG <- benchmark_ret[index(strategy_net_GOOG)]
comparison_GOOG <- na.omit(merge(strategy_net_GOOG, bench_GOOG))
colnames(comparison_GOOG) <- c("GOOG SAR+GARCH", "SPY Benchmark")

par(mfrow = c(1, 2))
barChart(GOOG['2024'], theme = 'white', main = "GOOG Price + SAR")
addSAR(accel = c(0.02, 0.2))
charts.PerformanceSummary(comparison_GOOG, main = "GOOG SAR+GARCH vs SPY")

# =========================
# AMZN STRATEGY
# =========================
amzn_price <- Cl(AMZN)
amzn_returns <- na.omit(diff(log(amzn_price)))

sar_amzn <- SAR(HLC(AMZN), accel = c(0.02, 0.2))
signal_amzn <- Lag(ifelse(Lag(amzn_price) < Lag(sar_amzn) & amzn_price > sar_amzn, 1,
                          ifelse(Lag(amzn_price) > Lag(sar_amzn) & amzn_price < sar_amzn, -1, 0)))
signal_amzn[is.na(signal_amzn)] <- 0

position_amzn <- rep(NA, length(signal_amzn))
position_amzn[1] <- 0
for (i in 2:length(signal_amzn)) {
  if (signal_amzn[i] == 1) {
    position_amzn[i] <- 1
  } else if (signal_amzn[i] == -1) {
    position_amzn[i] <- 0
  } else {
    position_amzn[i] <- position_amzn[i - 1]
  }
}
position_amzn <- xts(position_amzn, order.by = index(amzn_price))

spec_amzn <- ugarchspec(variance.model = list(model = "sGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "std")
fit_amzn <- ugarchfit(spec = spec_amzn, data = amzn_returns)
vol_amzn <- sigma(fit_amzn)
vol_thresh_amzn <- quantile(vol_amzn, 0.75, na.rm = TRUE)
vol_filter_amzn <- ifelse(vol_amzn < vol_thresh_amzn, 1, 0)
vol_filter_amzn <- xts(vol_filter_amzn, order.by = index(vol_amzn))

position_amzn <- position_amzn[index(vol_filter_amzn)] * vol_filter_amzn
strategy_ret_amzn <- position_amzn * amzn_returns[index(position_amzn)]
trades_amzn <- ifelse(Lag(position_amzn) != position_amzn, commission, 0)
strategy_net_amzn <- strategy_ret_amzn - trades_amzn
strategy_net_amzn[is.na(strategy_net_amzn)] <- 0

bench_amzn <- benchmark_ret[index(strategy_net_amzn)]
comparison_amzn <- na.omit(merge(strategy_net_amzn, bench_amzn))
colnames(comparison_amzn) <- c("AMZN SAR+GARCH", "SPY Benchmark")

par(mfrow = c(1, 2))
barChart(AMZN['2024'], theme = 'white', main = "AMZN Price + SAR")
addSAR(accel = c(0.02, 0.2))
charts.PerformanceSummary(comparison_amzn, main = "AMZN SAR+GARCH vs SPY")

