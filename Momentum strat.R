#Momentum Strat + garch vol filter
getSymbols(c("AAPL", "SPY"))

# ROC signal (Extract 1 logic)
roc <- ROC(Cl(AAPL), n = 25)
signal <- ifelse(roc > 0.05, 1,
                 ifelse(roc < -0.05, -1, 0))

ret <- diff(log(Cl(AAPL)))
ret <- na.omit(ret)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

fit <- ugarchfit(spec, data = ret)
vol <- sigma(fit)

# Volatility threshold filter (e.g., 75th percentile)
threshold <- quantile(vol, 0.75, na.rm = TRUE)
signal_filtered <- ifelse(vol < threshold, signal, 0)

strategy_ret <- signal_filtered * ret

# Commission costs
commission <- 0.001
trades <- ifelse(Lag(signal_filtered) != signal_filtered, commission, 0)
strategy_net <- strategy_ret - trades

ret.SPY <- diff(log(Cl(SPY)))
benchmark <- ret.SPY[index(strategy_net)]

comparison <- cbind(strategy_net, benchmark)
colnames(comparison) <- c("Momentum GARCH", "SPY Benchmark")


charts.PerformanceSummary(comparison)
table.AnnualizedReturns(comparison)
chart.Drawdown(comparison)

