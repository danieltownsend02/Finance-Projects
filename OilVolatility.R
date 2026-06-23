# Macro-Econometric Modeling: Brent Crude Volatility, Global Uncertainty, and OPEC Supply Dynamics

## Project Overview
This repository contains an empirical macroeconomic research framework implemented in R. The model isolates and quantifies the structural drivers of historical **Brent Crude Oil Volatility** by mapping the interactive relationships between global economic uncertainty, geopolitical risks, and cartel supply adjustments.

The analysis evaluates three primary exogenous macro channels:
1. **Global Economic Latency:** The World Uncertainty Index (WUI) fetched directly from the FRED database.
2. **Geopolitical Risk Ingestion:** The global Geopolitical Risk Index (GPR) mapped to track political and militarized stress shocks.
3. **Cartel Supply Interventions:** Historical OPEC supply data, smoothed via linear interpolation and first-differenced to isolate production adjustments.

---

## Technical Methodology & Econometric Structure

### 1. Data Harmonization & Feature Engineering
Financial and macroeconomic data frequently suffer from mismatched frequencies and non-stationarity. This script resolves those alignment friction points:
* **Volatility Extraction:** Daily returns for Brent Crude are computed from FRED data. A rolling 30-day standard deviation is generated to track conditional volatility, which is then aggregated into consistent monthly intervals.
* **Linear Supply Interpolation:** Because the historical OPEC production matrix is reported annually, the framework utilizes a fractional-year decimal mapping function (`approx`) to linearly interpolate annual supply changes into matching monthly time steps before converting the data to Millions of Barrels per Day (Mb/d).
* **Stationarity Transformation:** To prevent spurious regressions, the interpolated OPEC supply vector is transformed via a first difference ($\Delta \text{OPEC}$). Stationarity across all variables is mathematically verified using the **Augmented Dickey-Fuller (ADF) Test**.

### 2. Lagged Interaction Regression Model
To account for transmission delays in macroeconomic policy and energy infrastructure changes, the framework implements an Ordinary Least Squares (OLS) model featuring time-lagged variables and cross-product interactions:
* **Transmission Delay Mechanics:** The first difference of OPEC supply is lagged by 3 months ($k = 3$) to capture the realistic delayed impact of production changes on market stability.
* **Interaction Proxy:** An interaction term ($\text{W.Risk} \times \Delta \text{OPEC}_{\text{lag}}$) is constructed. This isolates how the relationship between OPEC supply adjustments and oil market volatility alters when under acute geopolitical stress.
* **The Structural Specification:** $$\text{Oil Volatility}_t = \beta_0 + \beta_1 \text{WUI}_t + \beta_2 \text{GPR}_t + \beta_3 \Delta \text{OPEC}_{t-3} + \beta_4 (\text{GPR}_t \times \Delta \text{OPEC}_{t-3}) + \epsilon_t$$

### 3. Statistical Diagnostics & Estimation Adjustments
To satisfy the strict classical linear regression assumptions required for institutional analysis, the system executes a comprehensive diagnostic battery:
* **Multicollinearity Checks:** The Variance Inflation Factor (`vif`) is calculated to guarantee that structural dependencies between the index metrics and the interaction proxies do not destabilize the parameter estimates.
* **Robust Inference Estimation:** To control for localized heteroskedasticity and residual clustering, the model executes a White-corrected robust standard error test via Heteroskedasticity-Consistent (`HC1`) covariance matrices (`vcovHC`).
* **Residual Diagnostics:** The script generates an evaluation matrix consisting of Actual vs. Predicted plots, Fitted vs. Residual scatters, normal Q-Q probability metrics, and error frequency histograms to verify residual normality.

---

## Technical Ecosystem & Dependencies
* **Data Processing & Frequency Matching:** `quantmod`, `xts`, `zoo`, `readxl`
* **Econometric Estimation & Testing:** `car`, `forecast`, `lmtest`, `sandwich`
* **Reporting Architecture:** `PerformanceAnalytics`, `stargazer`


----------------------------------------------------------------

#Finale Script:
#Packages:
library(car)
library(forecast)
library(lmtest)
library(PerformanceAnalytics)
library(quantmod)
library(readxl)
library(sandwich)
library(stargazer)
library(tseries)
library(xts)
library(zoo)

#Downloading Brent Data And Computing Quarterly Volatility
getSymbols("DCOILBRENTEU", src = "FRED")

#Returns Calculation and Volatility
BrentReturns <- dailyReturn(na.omit(DCOILBRENTEU))
Volatility30 <- runSD(BrentReturns, n=30)
Q.OilVolatiity <- apply.monthly(Volatility30, mean, na.rm=TRUE)

#Cleaning data
index(Q.OilVolatiity) <- as.Date(as.yearmon(index(Q.OilVolatiity)), frac =1 )
colnames(Q.OilVolatiity) <- "Q.OilVolatiity"

#Obtaining World Uncertainty Index from FRED database
getSymbols("WUIGLOBALWEIGHTAVG", src = "FRED")
W.Uncertainty <- na.omit(WUIGLOBALWEIGHTAVG)

#Cleaning Data
index(W.Uncertainty) <- as.Date(as.yearmon(index(W.Uncertainty)), frac = 1)
colnames(W.Uncertainty) <- "WUI"

#Geopolitical Risk Index (GPR) 
W.Risk.xls <- read_excel("Desktop/Data /GPR.xls")

#Cleaning Data
colnames(W.Risk.xls) <- c("Date", "W.Risk")
W.Risk <- xts(W.Risk.xls$W.Risk, order.by =as.Date(W.Risk.xls$Date))
index(W.Risk) <- as.Date(as.yearmon(index(W.Risk)), frac =1)

#OPEC and Interpolate
Opec <- read_excel("Desktop/Data /OPEC.xlsx", sheet = 1)

#Cleaning Data
colnames(Opec) <- c("Year","Supply.kbpd")
#kbpd is thousands barrels per day
Opec <- na.omit(Opec)

#Target Volatility Months
Target.months <- index(Q.OilVolatiity)

#Decimal year for Interpolation
decY <- as.numeric(format(Target.months, "%Y")) +
  (as.numeric(format(Target.months,"%m"))-1)/12

#Interpolation
OPEC.inter <-approx(
  x = Opec$Year,
  y = Opec$Supply.kbpd,
  xout = decY,
  rule = 2
)$y

#Converting to Mb/d 
OPEC.xts <- xts(OPEC.inter / 1000, order.by = Target.months)
#Cleaning Data 
colnames(OPEC.xts) <-"OPEC"


#First Difference of OPEC data to ensure Stationary
Diff.OPEC <- diff(OPEC.xts)
#Cleaning Data
colnames(Diff.OPEC) <- "Diff.OPEC"

#Merging data together into one XTS
Tmp.Merge1 <- merge(Q.OilVolatiity, W.Uncertainty)
Tmp.Merge2 <- merge(Tmp.Merge1, W.Risk)
Merged.XTS <- merge(Tmp.Merge2, Diff.OPEC)

Merged.XTS <- na.omit(Merged.XTS)
View(Merged.XTS)

#Regression 1 Data frame
dfregression1 <- data.frame(
  Date = index(Merged.XTS),
  coredata(Merged.XTS)
)

#Creating the Lagged OPEC Variable
OPEC.Oil.Lag <- lag(Diff.OPEC, k = 3)# 9 months

#Merging Data 2
OPEC.df.Lagged <- merge(Q.OilVolatiity, W.Uncertainty, W.Risk, OPEC.Oil.Lag)
#Clean Data and Renaming
colnames(OPEC.df.Lagged) <- c("Q.OilVolatiity", "WUI", "W.Risk", "OPEC.Oil.Lag")
OPEC.df.Lagged <- na.omit(OPEC.df.Lagged)

#Data frame
df.Lagged <- data.frame(Date = index(OPEC.df.Lagged), coredata(OPEC.df.Lagged))
laged.Model.1 <- lm(Q.OilVolatiity ~ W.Uncertainty + W.Risk + OPEC.Oil.Lag, data = OPEC.df.Lagged)

#Lagged testing Regression
summary(laged.Model.1)
adf.test(na.omit(OPEC.Oil.Lag))#Stationary

#Proxy between W.Risk and Lagged First Difference of OPEC
df.Lagged$W.Risk.OPEClag <- df.Lagged$W.Risk * df.Lagged$OPEC.Oil.Lag

#The Finale Regression Model
Final.Regression <- lm(Q.OilVolatiity ~ W.Uncertainty + W.Risk + OPEC.Oil.Lag + W.Risk.OPEClag, data = df.Lagged)
summary(Final.Regression)

#Diagnostics of Variables, Plotting
adf.test(W.Uncertainty)
adf.test(W.Risk)
adf.test(na.omit(Q.OilVolatiity))
adf.test(na.omit(OPEC.Oil.Lag))
adf.test(na.omit(df.Lagged$W.Risk.OPEClag))
#All Stationary

vif(Final.Regression) 

#Robust Standard Errors test
coeftest(Final.Regression, vcov = vcovHC(Final.Regression, type = "HC1"))

#Predicted vs Actual Values For Plotting
Predicted <- predict(Final.Regression)
Actual <- df.Lagged$Q.OilVolatiity

#Plot of data
plot(Actual, Predicted,
     xlab = "Actual Volatility",
     ylab = "Predicted Volatility",
     main = "Predicted vs Actual Volatility",
     pch = 19, col = "skyblue")
abline(0, 1, col ="black", lwd = 2) #Creates a 45-degree line

#Residuals creation 
Resid <- residuals(Final.Regression)

#Plotting Predicted VS Residuals
plot(Predicted, Resid,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 19, col = "skyblue")
abline(h = 0, col = "black", lwd = 2)

#QQ Plot 
qqnorm(Resid, main = "QQ Plot of Residuals")
qqline(Resid, col = "green", lwd = 1)

#Histogram of Residuals for clarity
hist(Resid,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "grey", breaks = 20)

plot(Volatility30, main = "Chart 1:
Rolling Volatility over a 30 day period")



