#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 15:31:35 2026

@author: danieltownsend
"""


import numpy as np
import pandas as pd
import yfinance as yf
from scipy.stats import zscore
from arch import arch_model
import statsmodels.api as sm
import matplotlib.pyplot as plt

# ==============================================================================
# 1. DATA ACQUISITION & PAIR CONFIGURATION 
# ==============================================================================
ticker_1 = "ALL"
ticker_2 = "GL"

print(f"Extracting pairs data ({ticker_1}, {ticker_2})...")
raw_data = yf.download([ticker_1, ticker_2, "SPY"], start="2016-01-01", auto_adjust=True, progress=False)

CloseP = raw_data['Close'][[ticker_1, ticker_2]].dropna()
spy_close = raw_data['Close']['SPY'].dropna()

test_start = "2019-01-01"
Test_Prices = CloseP.loc[CloseP.index >= test_start].copy()

# ==============================================================================
# 2. DYNAMIC LOOKBACK ROLLING ENGINE 
# ==============================================================================
lookback = 60  
rolling_beta = []
rolling_intercept = []

for i in range(len(Test_Prices)):
    current_date = Test_Prices.index[i]
    historical_window = CloseP.loc[CloseP.index < current_date].tail(lookback)
    
    if len(historical_window) < lookback:
        rolling_beta.append(np.nan)
        rolling_intercept.append(np.nan)
        continue
        
    X = sm.add_constant(historical_window[ticker_2])
    model = sm.OLS(historical_window[ticker_1], X).fit()
    rolling_beta.append(model.params[ticker_2])
    rolling_intercept.append(model.params['const'])

Test_Prices['Beta'] = rolling_beta
Test_Prices['Intercept'] = rolling_intercept
Test_Prices = Test_Prices.dropna()

# ==============================================================================
# 3. ASYMMETRIC SIGNAL LOGIC
# ==============================================================================
Test_Prices['Spread'] = Test_Prices[ticker_1] - Test_Prices['Intercept'] - Test_Prices['Beta'] * Test_Prices[ticker_2]

rolling_mean = Test_Prices['Spread'].rolling(window=30).mean()
rolling_std = Test_Prices['Spread'].rolling(window=30).std()
Test_Prices['Z_Score'] = (Test_Prices['Spread'] - rolling_mean) / rolling_std
Test_Prices = Test_Prices.dropna()

positions = []
current_pos = 0

for z in Test_Prices['Z_Score']:
    if current_pos == 0:
        if z > 1.5:
            current_pos = -1  
        elif z < -1.5:
            current_pos = 1   
    else:
        if (current_pos == -1 and z <= 0) or (current_pos == 1 and z >= 0):
            current_pos = 0
    positions.append(current_pos)

Test_Prices['Raw_Signal'] = positions

# ==============================================================================
# 4. APARCH DYNAMIC RISK MANAGEMENT (FIXED: Auto-Rescaling Activated)
# ==============================================================================
# Calculate raw dollar changes of the spread
Spread_Delta = Test_Prices['Spread'].diff().dropna()

# FIX: Set rescale=True to eliminate the DataScaleWarning completely
model_spec = arch_model(Spread_Delta, p=1, q=1, o=1, vol='aparch', dist='studentst', mean='AR', lags=1, rescale=True)
fit = model_spec.fit(disp='off')

# Extract internal scaling factor to return volatility to real-world units
scale_factor = fit.scale
volatility_series = pd.Series(fit.conditional_volatility / scale_factor, index=Spread_Delta.index).ffill()

inverse_vol = 1 / volatility_series
rolling_max_vol = inverse_vol.rolling(window=30, min_periods=1).max()
vol_scalar = inverse_vol / rolling_max_vol

aligned_signal = Test_Prices['Raw_Signal'].reindex(vol_scalar.index)
dynamic_positions = (aligned_signal * vol_scalar).ffill().fillna(0)

# ==============================================================================
# 5. ACCOUNTING & TRANSACTION COSTS (FIXED: Real-World Capital Scaling)
# ==============================================================================
# FIX: Measure daily spread dollar changes relative to the total capital value of the basket
Spread_Percentage_Returns = Spread_Delta / (Test_Prices[ticker_1] + Test_Prices['Beta'] * Test_Prices[ticker_2])
SPY_returns = np.log(spy_close / spy_close.shift(1)).loc[dynamic_positions.index]

Gross_Strat_Returns = (dynamic_positions.shift(1) * Spread_Percentage_Returns).dropna()

# Commission tracking friction logic
commission = 0.001
position_deltas = dynamic_positions.diff().abs()
costs_series = (position_deltas * commission).reindex(Gross_Strat_Returns.index, method='ffill').fillna(0)

Net_Strat_Returns = Gross_Strat_Returns - costs_series
compare = pd.DataFrame({'Strategy (Adaptive Net)': Net_Strat_Returns, 'SPY Benchmark': SPY_returns}).dropna()

# ==============================================================================
# 6. OUTPUT SUMMARIES
# ==============================================================================
portfolio_cumulative = (1 + compare['Strategy (Adaptive Net)']).cumprod() - 1
benchmark_cumulative = (1 + compare['SPY Benchmark']).cumprod() - 1

running_peak = (portfolio_cumulative + 1).cummax()
drawdown = ((portfolio_cumulative + 1) - running_peak) / running_peak
max_dd = drawdown.min() * 100

print("\n" + "="*45)
print(f"Final Cumulative Return (Adaptive Net): {portfolio_cumulative.iloc[-1]*100:.2f}%")
print(f"SPY Benchmark Return:                  {benchmark_cumulative.iloc[-1]*100:.2f}%")
print(f"Strategy Maximum Drawdown:             {max_dd:.2f}%")
print("="*45)

plt.style.use('seaborn-v0_8-darkgrid')

# --- PLOT 1: Adaptive Equity Curve vs Benchmark ---
plt.figure(figsize=(12, 4))
plt.plot(portfolio_cumulative * 100, color='darkblue', label='Adaptive Strategy (NET)')
plt.plot(benchmark_cumulative * 100, color='gray', linestyle='--', label='SPY Benchmark')
plt.title(f"Adaptive Out of Sample Strategy ({ticker_1}/{ticker_2}) vs SPY")
plt.ylabel("Cumulative Returns (%)")
plt.legend()
plt.show()

# --- PLOT 2: Z-Score Spread ---
plt.figure(figsize=(12, 4))
plt.plot(Test_Prices.index, Test_Prices['Z_Score'].values, color='black', linewidth=1, label='Z-score')
plt.axhline(1.5, color='gray', linestyle='--')
plt.axhline(0, color='gray', linestyle=':')
plt.axhline(-1.5, color='gray', linestyle='--')
plt.fill_between(Test_Prices.index, Test_Prices['Z_Score'].values, 0, where=(Test_Prices['Raw_Signal'] == -1), color='red', alpha=0.3)
plt.fill_between(Test_Prices.index, Test_Prices['Z_Score'].values, 0, where=(Test_Prices['Raw_Signal'] == 1), color='green', alpha=0.3)
plt.title("Residual Spread Z-Score & Active States")
plt.show()

# --- PLOT 3: Volatility Time Series ---
plt.figure(figsize=(10, 3))
plt.plot(volatility_series.index, volatility_series.values, color='purple')
print(f"Mean Realized Spread Volatility: {volatility_series.mean():.4f}")
plt.title("apARCH Estimated Volatility (Original Scale)")
plt.show()

# --- PLOT 4: Clean Asset Closing Prices ---
plt.figure(figsize=(12, 4))
plt.plot(raw_data['Close'][ticker_1], color='skyblue', label=ticker_1)
plt.plot(raw_data['Close'][ticker_2], color='maroon', label=ticker_2)
plt.title(f"Closing Prices of {ticker_1} and {ticker_2}")
plt.legend()
plt.show()