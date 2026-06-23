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



