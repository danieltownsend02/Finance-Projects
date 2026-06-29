#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 14:21:25 2026

@author: danieltownsend
"""

import numpy as np
import pandas as pd
import yfinance as yf
import statsmodels.api as sm
from statsmodels.tsa.stattools import coint
import matplotlib.pyplot as plt

# ==============================================================================
# 1. DATA ACQUISITION & ALIGNMENT PIPELINE
# ==============================================================================
tickers = ["BP.L", "SHEL.L"]
print(f"Fetching live historical data for pairs model: {tickers}...")

# Download data over a 3-year historical evaluation timeline
raw_data = yf.download(tickers, period="3y", interval="1d", progress=False)

# Clean and flatten columns to avoid pandas multi-index lookup failure
if isinstance(raw_data.columns, pd.MultiIndex):
    asset_y = raw_data['Close']['BP.L'].dropna()
    asset_x = raw_data['Close']['SHEL.L'].dropna()
else:
    asset_y = raw_data['BP.L'].dropna()
    asset_x = raw_data['SHEL.L'].dropna()

# Inner-join indices to eliminate any localized market holiday mismatches
df = pd.concat([asset_y, asset_x], axis=1, keys=['BP', 'SHELL']).dropna()

# ==============================================================================
# 2. ENGLE-GRANGER COINTEGRATION FRAMEWORK (THE EXACT MATH IN R)
# ==============================================================================
# Run the two-step cointegration test directly
# H0: The residual spread is non-stationary (No cointegration relationship exists)
score, p_value, _ = coint(df['BP'], df['SHELL'], trend='c', autolag='AIC')

print("\n=== SYSTEMATIC STATISTICAL TESTS ===")
print(f"Engle-Granger Cointegration t-stat : {score:.4f}")
print(f"P-Value                             : {p_value:.4f}")

if p_value < 0.05:
    print(">>> SUCCESS: Pair is statistically cointegrated at the 95% threshold.")
else:
    print(">>> WARNING: Pair lacks a robust cointegrating anchor. Trade tracking carrying higher risk.")

# ==============================================================================
# 3. OLS MATRIX REGRESSION: HEDGE RATIO EXTRAPOLATION
# ==============================================================================
# Find the exact dynamic capital relationship: Asset_Y = Hedge_Ratio * Asset_X + Intercept
X = sm.add_constant(df['SHELL'])
ols_model = sm.OLS(df['BP'], X).fit()
hedge_ratio = ols_model.params['SHELL']
intercept = ols_model.params['const']

# Calculate raw spread residuals
df['Spread'] = df['BP'] - (hedge_ratio * df['SHELL'] + intercept)

print(f"\nCalculated Hedge Ratio (Beta)      : {hedge_ratio:.4f}")
print(f"Calculated Structural Intercept    : {intercept:.4f}")

# ==============================================================================
# 4. SIGNAL FREQUENCY LOOKUP (ROLLING Z-SCORE ENGINE)
# ==============================================================================
window = 30  # 30-day rolling mean and standard deviation horizon
df['Spread_Mean'] = df['Spread'].rolling(window=window).mean()
df['Spread_Std'] = df['Spread'].rolling(window=window).std()
df['Z_Score'] = (df['Spread'] - df['Spread_Mean']) / df['Spread_Std']

# Clean transient NaN elements caused by the rolling calculation window
df = df.dropna()

# Establish trading thresholds (Standard deviation units)
upper_entry = 2.0
lower_entry = -2.0
exit_line = 0.0

# ==============================================================================
# ADDITION: SYSTEMATIC BACKTEST ENGINE & PERFORMANCE TRACKER
# ==============================================================================
df['Signal'] = 0
df.loc[df['Z_Score'] >= upper_entry, 'Signal'] = -1  # Short Spread Trigger
df.loc[df['Z_Score'] <= lower_entry, 'Signal'] = 1   # Long Spread Trigger

# Replicate R position holding logic: Persist active entries until mean reversion (0)
# This replaces 0 values with NaN so forward-fill maintains your active trade state
df['Position'] = df['Signal'].replace(0, np.nan).ffill().fillna(0)

# Calculate daily asset return profile of the spread portfolio basket
# Normalizes the spread movement relative to total capital size of the leg assets
df['Spread_Daily_Return'] = df['Spread'].diff() / (df['BP'] + (abs(hedge_ratio) * df['SHELL']))
df['Strategy_Return'] = df['Position'].shift(1) * df['Spread_Daily_Return']
df['Cumulative_Return'] = (1 + df['Strategy_Return'].fillna(0)).cumprod() - 1

# ==============================================================================
# 5. DIAGNOSTICS SEPARATED VISUALIZATION LAYER
# ==============================================================================
plt.style.use('seaborn-v0_8-darkgrid')

# --- PLOT 1: ROLLING COINTEGRATED RESIDUAL SPREAD TRACKING ---
plt.figure(figsize=(12, 4))
plt.plot(df.index, df['Spread'], color='purple', label='Asset Residual Spread', linewidth=1.2)
plt.axhline(df['Spread'].mean(), color='black', linestyle='--', label='Long-Term Equilibrium Mean')
plt.title("Statistical Arbitrage Spread Trajectory (BP vs. Shell)", fontsize=12, fontweight='bold')
plt.xlabel("Timeline Horizon")
plt.ylabel("Spread Deviation Value")
plt.legend(loc="upper left")
plt.tight_layout()
plt.show()

# --- PLOT 2: REAL-TIME STRATEGY Z-SCORE MATRIX SIGNALS ---
plt.figure(figsize=(12, 4))
plt.plot(df.index, df['Z_Score'], color='teal', label='Rolling Spread Z-Score', linewidth=1.2)

# Structural trading bounds overlays
plt.axhline(upper_entry, color='crimson', linestyle='--', linewidth=1.5, label='Short Spread Entry Threshold (+2σ)')
plt.axhline(lower_entry, color='forestgreen', linestyle='--', linewidth=1.5, label='Long Spread Entry Threshold (-2σ)')
plt.axhline(exit_line, color='black', linestyle=':', alpha=0.5, label='Mean Reversion Mean Exit Zone (0.0)')

# Highlight active execution points
long_signals = df[df['Z_Score'] <= lower_entry]
short_signals = df[df['Z_Score'] >= upper_entry]

plt.scatter(long_signals.index, long_signals['Z_Score'], color='green', marker='^', s=40, label='Long Position Trigger (Buy BP / Short Shell)')
plt.scatter(short_signals.index, short_signals['Z_Score'], color='red', marker='v', s=40, label='Short Position Trigger (Short BP / Buy Shell)')

plt.title("Systematic Execution Engine: Dynamic Signal Map", fontsize=12, fontweight='bold')
plt.xlabel("Timeline Horizon")
plt.ylabel("Z-Score Units")
plt.legend(loc="lower left")
plt.tight_layout()
plt.show()

# --- PLOT 3: STRATEGY PERFORMANCE CURVE (NEW EQUITY GENERATION TRACKER) ---
plt.figure(figsize=(12, 4))
plt.plot(df.index, df['Cumulative_Return'] * 100, color='forestgreen', linewidth=2, label='Pairs Trading Net Return')
plt.axhline(0, color='black', linestyle='--', alpha=0.5)
plt.title("Systematic Strategy Performance: Cumulative Equity Growth", fontsize=12, fontweight='bold')
plt.xlabel("Timeline Horizon")
plt.ylabel("Cumulative Returns (%)")
plt.legend(loc="upper left")
plt.tight_layout()
plt.show()