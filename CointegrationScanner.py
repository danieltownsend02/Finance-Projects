#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 15:11:01 2026

@author: danieltownsend
"""

import yfinance as yf
from statsmodels.tsa.stattools import coint
import pandas as pd

def scan_all_pairs(tickers, start="2020-01-01"):
    # Fetch data once to save time
    print(f"Downloading data for {len(tickers)} tickers...")
    data = yf.download(tickers, start=start, progress=False)['Close'].dropna()
    
    results = []
    n = len(tickers)
    
    print("Scanning for cointegration...")
    for i in range(n):
        for j in range(i + 1, n):
            s1, s2 = tickers[i], tickers[j]
            # Use Engle-Granger test
            score, pvalue, _ = coint(data[s1], data[s2])
            
            # Store everything to help you analyze the "almost" pairs
            results.append({'Asset1': s1, 'Asset2': s2, 'p-value': pvalue})
    
    # Return sorted by p-value (lowest is most cointegrated)
    df_results = pd.DataFrame(results).sort_values('p-value')
    return df_results

# Example: A diverse list of highly correlated mega-caps & sectors
tickers = ["KO", "PEP", "XOM", "CVX", "JPM", "BAC", "MSFT", "AAPL", "GOOGL", "AMZN", "TLT", "IEF"]
df = scan_all_pairs(tickers)

# Display top 20 most cointegrated pairs
print(df.head(20).to_string(index=False))