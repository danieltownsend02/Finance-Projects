# Trend-Following Trading System: Multi-SMA Crossover with Dynamic sGARCH Volatility Regime Muting

## Project Overview
This repository contains an algorithmic trend-following strategy implemented in R that trades **Apple (AAPL)** against the **S&P 500 ETF (SPY)** market benchmark. 

The strategy relies on a classic dual Simple Moving Average (20-day and 50-day SMA) crossover framework to catch major asset trends. However, to prevent severe drawdowns and "whipsaws" during volatile market transitions, the core execution layer features a mathematical **sGARCH risk filter using a Student-t distribution** to entirely freeze trading when localized asset risk spikes.

---

## Core Mechanics & Strategy Logic

### 1. Trend Identification Layers
* **SMA Crossover Framework:** The system evaluates 20-period and 50-period Simple Moving Averages. A **Long Signal (1)** is generated when the faster 20-day SMA crosses above the slower 50-day SMA, indicating upward momentum. The position is **Liquidated to Cash (0)** when the 20-day SMA falls below the 50-day SMA.
* **Execution Delays (Lag):** To ensure a valid, realistic backtest and eliminate look-ahead bias, all execution triggers are strictly lagged by 1 period so that trades are executed at the next open rather than on historical close data.

### 2. Microstructure Risk Control via sGARCH
Trend-following strategies historically lose capital during highly volatile, choppy, sideways-moving markets. This architecture resolves that weakness by tracking mathematical volatility clustering:
* **Volatility Modeling:** A standard GARCH (`sGARCH`) framework is fitted to daily log-returns using a **Student-t distribution (`std`)** to capture heavy tails and real-world shock risks.
* **The 75th Percentile Mute Switch:** The code extracts conditional volatility ($\sigma$) and calculates its historical 75th percentile. If current asset volatility spikes into the top 25% of all historical regimes, the system flags the market as a high-risk zone, overrides the trend-following signals, and **forces the strategy into cash** to preserve portfolio equity.

### 3. Realistic Friction Penalization
* The model enforces a **10-basis-point (0.1%) commission charge** whenever the portfolio adjusts its position. 
* Factoring transaction costs directly into the return matrix guarantees that performance analytics account for realistic market execution friction, rather than presenting a theoretical academic ideal.

---

## Performance Metrics & Analysis
The final strategy performance and downside protections are evaluated using the `PerformanceAnalytics` toolbox:
* **Annualized Returns:** Out-of-sample comparison of net strategy performance against a long-only SPY benchmark.
* **Sortino Ratio:** Evaluation of downside risk-adjusted returns, measuring outperformance relative to downside deviation.
* **Maximum Drawdown (MDD):** Analysis of peak-to-trough equity curve preservation, evaluating the defensive effectiveness of the GARCH volatility ceiling.

---

## Technical Ecosystem & Dependencies
* **Data Processing & Analytics:** `quantmod`, `TTR`, `xts`
* **Volatility Modeling Frameworks:** `rugarch`
* **Portfolio Attribution:** `PerformanceAnalytics`


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

