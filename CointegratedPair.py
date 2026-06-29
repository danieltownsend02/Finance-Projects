#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 14:37:12 2026

@author: danieltownsend
"""

!pip install numpy pandas yfinance scipy scikit-learn arch statsmodels matplotlib

import numpy as np
import pandas as pd
import yfinance as yf
from scipy.stats import zscore
from sklearn.mixture import GaussianMixture
from arch import arch_model
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.vector_ar.vecm import coint_johansen
import matplotlib.pyplot as plt

# ==============================================================================
# DATA ACQUISITION
# ==============================================================================
print("Extracting pairs data (AAPL, GOOG) and SPY benchmark...")
raw_data = yf.download(["AAPL", "GOOG", "SPY"], start="2010-01-01", progress=False)

# Obtaining Closing prices
CloseP = raw_data['Close'][['AAPL', 'GOOG']].dropna()
spy_close = raw_data['Close']['SPY'].dropna()

# ==============================================================================
# TRAIN / TEST SPLIT
# ==============================================================================
training_end = "2018-12-31"
Training_Clprices = CloseP.loc[CloseP.index <= training_end]
Test_Prices = CloseP.loc[CloseP.index > training_end]

# ==============================================================================
# COINTEGRATION TEST (ENGLE-GRANGER)
# ==============================================================================
X_train = sm.add_constant(Training_Clprices['GOOG'])
Cointegration_Model = sm.OLS(Training_Clprices['AAPL'], X_train).fit()
Training_Resids = Cointegration_Model.resid

# ADF Test
adf_result = adfuller(Training_Resids, maxlag=1, regression='n') # 'n' = no trend/constant (matching type="none")
print(f"\nADF Test Statistic on Residuals: {adf_result[0]:.4f}")
print(f"p-value: {adf_result[1]:.4f}")

# ==============================================================================
# SIGNAL GENERATION
# ==============================================================================
# Generate spread residuals on testing data using training coefficients
Test_Resids = Test_Prices['AAPL'] - Cointegration_Model.params['const'] - Cointegration_Model.params['GOOG'] * Test_Prices['GOOG']

# Z-score standardization
z_score = pd.Series(zscore(Test_Resids), index=Test_Prices.index)

# Short if high z-score, long if low
signal = np.where(z_score > 1.0, -1, np.where(z_score < -1.0, 1, 0))
signal = pd.Series(signal, index=Test_Prices.index)

# ==============================================================================
# VOLATILITY REGIME FILTERING (EXPECTATION-MAXIMIZATION MIXTURE MODEL)
# ==============================================================================
SPYR = spy_close.pct_change().dropna()

# Setting seed for replication
gmm = GaussianMixture(n_components=2, weights_init=[0.8, 0.2], random_state=123)
gmm.fit(SPYR.values.reshape(-1, 1))

# Extract regime assignments
regime = gmm.predict(SPYR.values.reshape(-1, 1))
regime_series = pd.Series(regime, index=SPYR.index)

# Identify the calm regime index (the one with the higher cluster allocation weight/lambda)
calm_regime_idx = np.argmax(gmm.weights_)
regimeFilter = np.where(regime_series == calm_regime_idx, 1, 0)
regime_Filter_Series = pd.Series(regimeFilter, index=SPYR.index)

# Align to testing horizon data
regime_Aligned = regime_Filter_Series.reindex(Test_Prices.index, method='ffill').fillna(0)

# Plotting Regime Probability Densities
calm_Returns = SPYR[regime_Filter_Series == 1]
volatile_Returns = SPYR[regime_Filter_Series == 0]

plt.figure(figsize=(10, 5))
plt.hist(volatile_Returns, bins=50, density=True, alpha=0.5, color='red', label='Volatile')
plt.hist(calm_Returns, bins=50, density=True, alpha=0.5, color='green', label='Calm')
plt.title("Regime Return Distributions")
plt.xlabel("Returns")
plt.ylabel("Density")
plt.legend()
plt.show()

# ==============================================================================
# POSITION CREATION & APARCH MODELLING
# ==============================================================================
pos = np.where((signal != 0) & (regime_Aligned == 1), signal, 0)
pos = pd.Series(pos, index=Test_Prices.index).ffill().fillna(0)

Spread_Ret = Test_Resids.diff().dropna()

# Fit an apARCH(1,1) with Student-t distribution
# Note: arch library models apARCH using power=1 or power=2 with asymmetry adjustments
model_spec = arch_model(Spread_Ret * 100, p=1, q=1, o=1, vol='aparch', dist='studentst', mean='AR', lags=2)
fit = model_spec.fit(disp='off')
print(fit.summary())

# ==============================================================================
# GARCH VOLATILITY WEIGHTING & STRATEGY RETURNS
# ==============================================================================
volatility = fit.conditional_volatility / 100
volatility_series = pd.Series(volatility, index=Spread_Ret.index).ffill()

volatility_norm = 1 / volatility_series
volatility_norm = volatility_norm / volatility_norm.max()

# Align vectors
aligned_signal = signal.reindex(volatility_norm.index)
signal_weighted = (aligned_signal * volatility_norm).ffill().fillna(0)

# Return calculations
AAPL_returns = np.log(Test_Prices['AAPL'] / Test_Prices['AAPL'].shift(1))
GOOG_returns = np.log(Test_Prices['GOOG'] / Test_Prices['GOOG'].shift(1))
SPY_returns = np.log(spy_close / spy_close.shift(1)).loc[signal_weighted.index]

Gweighted_pair = (signal_weighted * (AAPL_returns - GOOG_returns)).dropna()

# Commission logic penalty matrix
commission = 0.001
costs = np.where(pos.shift(1) != pos, commission, 0)
costs_series = pd.Series(costs, index=pos.index).reindex(Gweighted_pair.index, method='ffill').fillna(0)

Net_Strat = Gweighted_pair - costs_series
compare = pd.DataFrame({'Strategy (NET)': Net_Strat, 'SPY Benchmark': SPY_returns}).dropna()

# ==============================================================================
# PERFORMANCE LOG SUMMARY
# ==============================================================================
portfolio_cumulative = (1 + compare['Strategy (NET)']).cumprod() - 1
benchmark_cumulative = (1 + compare['SPY Benchmark']).cumprod() - 1

# Max Drawdown Calculation
running_peak = (portfolio_cumulative + 1).cummax()
drawdown = ((portfolio_cumulative + 1) - running_peak) / running_peak
max_dd = drawdown.min() * 100

print("\n" + "="*45)
print(f"Final Cumulative Return (NET): {portfolio_cumulative.iloc[-1]*100:.2f}%")
print(f"SPY Benchmark Return:         {benchmark_cumulative.iloc[-1]*100:.2f}%")
print(f"Strategy Maximum Drawdown:    {max_dd:.2f}%")
print("="*45)

# Plot Performance Summary
plt.figure(figsize=(12, 5))
plt.plot(portfolio_cumulative * 100, color='darkblue', label='Strategy (NET)')
plt.plot(benchmark_cumulative * 100, color='gray', linestyle='--', label='SPY Benchmark')
plt.title("Out of Sample Strategy vs SPY Benchmark")
plt.ylabel("Cumulative Returns (%)")
plt.legend()
plt.show()

# ==============================================================================
# JOHANSEN COINTEGRATION TEST
# ==============================================================================
# Detrend/constant handling matches ecdet="none", lags K=2 matches k_ar_diff=1 in statsmodels
jo_result = coint_johansen(Training_Clprices, det_order=0, k_ar_diff=1)
print("\nJohansen Trace Statistic:", jo_result.lr1)
print("Johansen Critical Values (90%, 95%, 99%):\n", jo_result.cvt)

# ==============================================================================
# SIGNAL POSITIONS PLOTTED (POLYGON REPLICATION)
# ==============================================================================
plt.figure(figsize=(12, 5))
plt.plot(z_score.index, z_score.values, color='black', linewidth=1, label='Z-score')
plt.axhline(1, color='gray', linestyle='--')
plt.axhline(0, color='gray', linestyle=':')
plt.axhline(-1, color='gray', linestyle='--')

# Fill Short Signals (z > 1)
plt.fill_between(z_score.index, z_score.values, 0, where=(z_score.values > 1), color='red', alpha=0.3, label='Short Signal (z > 1)')
# Fill Long Signals (z < -1)
plt.fill_between(z_score.index, z_score.values, 0, where=(z_score.values < -1), color='green', alpha=0.3, label='Long Signal (z < -1)')

plt.title("Residual Spread")
plt.ylabel("Z-score")
plt.legend(loc='upper right')
plt.show()

# ==============================================================================
# DIAGNOSTIC FORECASTS & HISTORICAL PRICES
# ==============================================================================
# Forecast
forecast = fit.forecast(horizon=5, method='simulation')
print("\nForecasted Conditional Volatility (Next 5 Days):\n", np.sqrt(forecast.variance.iloc[-1]))

# Estimated Volatility
plt.figure(figsize=(10, 4))
plt.plot(volatility_series.index, volatility_series.values, color='purple')
plt.title("Estimated volatility for Net Strategy")
plt.show()

# Closing Prices Plot
plt.figure(figsize=(12, 5))
plt.plot(raw_data['Close']['AAPL'], color='skyblue', label='AAPL')
plt.plot(raw_data['Close']['GOOG'], color='maroon', label='GOOG')
plt.title("Closing Prices of AAPL and GOOG")
plt.legend()
plt.show()
