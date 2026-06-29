#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 15:25:19 2026

@author: danieltownsend
"""

import yfinance as yf
import pandas as pd
import requests
from io import StringIO
from statsmodels.tsa.stattools import coint
from itertools import combinations
import time

# 1. Fetch S&P 500 Universe
def get_sp500_info():
    url = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/91.0.4472.124 Safari/537.36"}
    response = requests.get(url, headers=headers)
    df = pd.read_html(StringIO(response.text))[0]
    df['Symbol'] = df['Symbol'].str.replace('.', '-')
    return df[['Symbol', 'GICS Sector']]

# 2. Optimized Scanner with Data Alignment
def scan_universe(universe_df):
    tickers = universe_df['Symbol'].tolist()
    print("Downloading 2-year price history for S&P 500...")
    
    # Download data as a flat table
    data = yf.download(tickers, period="2y", auto_adjust=True, progress=True)['Close']
    
    results = []
    unique_sectors = universe_df['GICS Sector'].unique()
    
    for sector in unique_sectors:
        sector_tickers = universe_df[universe_df['GICS Sector'] == sector]['Symbol'].tolist()
        # Filter for tickers present in data
        valid = [t for t in sector_tickers if t in data.columns and not data[t].dropna().empty]
        
        if len(valid) < 2: 
            continue
        
        print(f"Scanning {sector} sector...")
        for s1, s2 in combinations(valid, 2):
            # Stage 1: Correlation Filter
            if data[s1].corr(data[s2]) > 0.85:
                # Stage 2: Synchronized Data Alignment
                pair_df = pd.concat([data[s1], data[s2]], axis=1).dropna()
                
                # Check for sufficient data points
                if len(pair_df) < 100: continue
                
                # Stage 3: Cointegration Test
                score, pvalue, _ = coint(pair_df.iloc[:, 0], pair_df.iloc[:, 1])
                
                if pvalue < 0.01:
                    results.append({'Pair': f"{s1}/{s2}", 'Sector': sector, 'p-value': pvalue})
        
        time.sleep(0.5)
    
    return pd.DataFrame(results).sort_values('p-value')

# 3. Execution
if __name__ == "__main__":
    sp500_info = get_sp500_info()
    df_top = scan_universe(sp500_info)
    
    print("\n" + "="*45)
    print("--- TOP 5 MOST COINTEGRATED PAIRS ---")
    print("="*45)
    if not df_top.empty:
        print(df_top.head(5).to_string(index=False))
    else:
        print("No pairs found matching the criteria.")