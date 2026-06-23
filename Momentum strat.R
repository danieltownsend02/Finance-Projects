# Quantitative Momentum Architecture: Rate of Change (ROC) Strategy with Dynamic sGARCH Risk-Muting Filter

## Project Overview
This repository hosts an algorithmic asset allocation framework written in R that executes a absolute **Price Momentum (Rate of Change)** strategy on **Apple (AAPL)** relative to the **S&P 500 ETF (SPY)** market benchmark.

The core logic shifts away from lag-heavy moving averages to evaluate pure price velocity. It isolates strong directional breakouts over a rolling 25-day period. To counter the risk of severe downside drawdowns during sudden market reversals, the execution layer incorporates a univariate **Standard GARCH (sGARCH) volatility filter with a Student-t distribution** to act as an automated capital preservation switch.

---

## Core System Architecture & Logic



### 1. Velocity-Based Signal Layer
* **Rate of Change (ROC) Metric:** The model computes a rolling 25-period Rate of Change on closing prices to track momentum velocity. 
* **Directional Threshold Triggers:** * A **Long Expansion Signal (1)** is generated if Apple's price gains more than **5%** over the rolling window ($ROC > 0.05$).
  * A **Short Contraction Signal (-1)** is generated if Apple's price declines more than **5%** over the rolling window ($ROC < -0.05$).
  * The strategy shifts completely **Flat to Cash (0)** if momentum remains range-bound between $-5\%$ and $+5\%$.

### 2. Time-Series Risk Control via sGARCH
High-momentum breakouts are highly vulnerable to abrupt, high-volatility trend reversals. This framework actively limits that exposure through an econometric tail-risk ceiling:
* **Volatility Modeling Framework:** A univariate Standard GARCH (`sGARCH`) process models the time-varying variance of daily log-returns. The framework utilizes a **Student-t distribution (`std`)** to capture realistic fat tails and asymmetric shock profiles.
* **The 75th Percentile Dynamic Cutoff:** The code measures conditional volatility ($\sigma$) and determines its historical 75th percentile. If localized asset risk enters the top 25% boundary of historical variance, the model flag spikes, overrides the momentum indicators, and **forces the strategy to a zero-exposure cash position** to protect capital.

### 3. Execution Friction & Performance Tracking
* **Friction Penalty:** The strategy enforces a **10-basis-point (0.1%) commission slippage penalty** across all allocation transitions. This penalizes excessive trading churn and guarantees that backtested performance matches realistic net execution profiles rather than idealized math.
* **Attribution Suite:** Returns are merged with the SPY benchmark and processed via `PerformanceAnalytics` to isolate institutional performance metrics, featuring rolling historical return performance summaries and maximum peak-to-trough equity drawdowns.

---

## Technical Ecosystem & Dependencies
* **Data Processing & Analytics:** `quantmod`, `TTR`, `xts`
* **Volatility Modeling Frameworks:** `rugarch`
* **Portfolio Attribution:** `PerformanceAnalytics`

-----------------------------------------------------------------------

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

