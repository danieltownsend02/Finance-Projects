#Install Packages
install.packages("mixtools")
install.packages("DistributionUtils", type = "binary", repos = "https://cloud.r-project.org") #Only way "ruGARCH" would install as it would say it is required.
install.packages("GeneralizedHyperbolic", repos = "https:// cloud.r-project.org") #required for "ruGARCH" and "urca" would not install packages without this.
install.packages("urca")

#Load Libraries
library(quantmod)
library(PerformanceAnalytics)
library(urca)
library(DistributionUtils)
library(GeneralizedHyperbolic)
library(rugarch)
library(mixtools)
library(tseries)

#Cointegration Strategy with regime- filtered trading during periods of calmness
#Based on Cointegration, mean reverting spread signals with volatility regime filtering
#Model adds commission costs to reflect real life trading

#Extract pairs data AAPL (Apple) and GOOG (Alphabet, googles parent company), extract SPY as benchmark
getSymbols(c("AAPL", "GOOG", "SPY"), from = "2010-01-01")

#Obtaining Closing prices
CloseP <- na.omit(merge(Cl(AAPL), Cl(GOOG)))
colnames(CloseP) <- c("AAPL", "GOOG")

#Train/Test Split
#Using past data from(2010-2018) to train model and test the performance on future data
# from (2019-2025). This is to avoid looking-ahead Bias

Training.End <- as.Date("2018-12-31")
Training.Clprices <- CloseP[index(CloseP) <= Training.End]
Test.Prices <- CloseP[index(CloseP) > Training.End]

#Cointegration Test (Engle-Granger)
# identify the long-term relationship between AAPL - GOOG
Cointegration.Model <- lm(Training.Clprices$AAPL ~ Training.Clprices$GOOG)
Training.Resids <- residuals(Cointegration.Model)
#ADF Test
summary(ur.df(Training.Resids, type = "none"))
#Reject null confirming stationary, no unit root

#Signal Generation, generating signals based on residual spread deviation from the mean.
#when z-score threshold passes 1 and -1 trades are made
Test.Resids <- Test.Prices$AAPL - coef(Cointegration.Model)[1] - coef(Cointegration.Model)[2] * Test.Prices$GOOG
#Z-score standardized to mean 0, SD 1
z.score <- scale(Test.Resids)
#Short if high z-score, long if low
signal <- ifelse(z.score > 1.0, -1, ifelse(z.score <-1.0, 1, 0))
#Formatting
signal <- xts(signal, order.by = index(Test.Prices))

#Volatility Regime Filtering
#SPY benchmark distribution used to classify regimes ie periods of calm vs volatile
#Use of Expectation- Maximisation model from "mixtools"
SPYR <- na.omit(dailyReturn(Cl(SPY)))

#Setting seed for replication
set.seed(123)
mix.mod <-normalmixEM(SPYR, k = 2, lambda = c(0.8, 0.2))
#Regime creation
regime <- apply(mix.mod$posterior, 1, which.max)
#Formatting
regime.xts <- xts(regime, order.by = index(SPYR))
#Filter and alignment
regimeFilter <- ifelse(regime.xts == which.max(mix.mod$lambda), 1, 0)
#Keeps only the Calm regime
regime.Aligned <- regimeFilter[index(Test.Prices)]
#Summary of Mix.mod
summary(mix.mod)

#Plotting Regime Probability Densities for Clarity
calm.Returns <- SPYR[which(regimeFilter == 1)]
volatile.Returns <-SPYR[which(regimeFilter == 0)]

#Density Plot
#Adding Volatile Regime
plot(density(volatile.Returns), col = rgb(1, 0, 0, 0.5), main = "Regime Return Distributions",
     xlab = "Returns",
     ylim = c(0, max(density(volatile.Returns)$y, density(calm.Returns)$y)))
polygon(density(volatile.Returns), col = rgb(1, 0, 0, 0.5), border = "red")
#Adding Calm Regime
lines(density(calm.Returns), col = rgb(0, 1, 0, 0,5))
polygon(density(calm.Returns), col = rgb(0, 1, 0, 0.5), border = "green")
legend("topleft", legend = c("Volatile", "Calm"), fill = c(rgb(1, 0, 0, 0.5), 
                                                            rgb(0, 1, 0, 0.5)))
#Position creation
#Combining signal and regime filter
#only trade when signal and regime show trade
pos <- ifelse(signal != 0 & regime.Aligned == 1, signal, 0)
pos <- na.locf(pos, na.rm = FALSE)
pos[is.na(pos)] <- 0

#GARCH Volatility Weighting
Spread.Ret <- na.omit(diff(Test.Resids))
spec <- ugarchspec()
show(spec)
fit <- ugarchfit(spec = spec, data = Spread.Ret)
show(fit)#Fat tails found
#alpha1 and beta1 significant
#ar1 and ma1 not significant
#poor goodness of fit

#Using apARCH to the residual spread with Student-t distribution for improved results
spec <- ugarchspec(variance.model = list(model = "apARCH"),
                   mean.model = list(armaOrder = c(2, 2)),
                   distribution.model = "std")
fit<- ugarchfit(spec = spec, data = Spread.Ret)
show(fit)
# no autocorrelation
#omega and gamma1 shows parameter instability
# sign bias suggests no leverage effect
#good goodness of fit
# mean of spread suggests positive drift
#ar1, ar2 suggests oscillatory behaviour
#gamma 1 not statistically significant
#shape suggests fat tails
#ljung suggets no autocorrelation
#joint stat, omega and gamma1 unstable
#no sign bias

#Continuing GARCH Weighting
volatility <- sigma(fit)
#Alignment
volatility.xts <- xts(volatility, order.by = index(Spread.Ret))
volatility.xts <- na.locf(volatility.xts)

volatility.norm <- 1 / volatility.xts
volatility.norm <- volatility.norm / max(volatility.norm, na.rm = TRUE)
aligned.signal <- signal[index(volatility.norm)]

#Creation of Weighted Signal
signal.weighted <- aligned.signal * volatility.norm
signal.weighted <- na.locf(signal.weighted, na.rm = FALSE)
signal.weighted[is.na(signal.weighted)] <- 0

#Return Calculations
#spread return ( AAPL - GOOG)
APPL.returns <- diff(log(Test.Prices$AAPL))
GOOG.returns <- diff(log(Test.Prices$GOOG))
SPY.returns <- na.omit(diff(log(Cl(SPY))))
#Pair weighted
Gweighted.pair <- signal.weighted * (APPL.returns - GOOG.returns)
Gweighted.pair <- na.omit(Gweighted.pair)

#Commission
#to be more realistic penalize trading frequency with commission to mimic real life
commission <- 0.001
costs <- ifelse(lag(pos) != pos, commission, 0)
Net.Strat <- Gweighted.pair - costs[index(Gweighted.pair)]
Net.Strat <- na.omit(Net.Strat)

#SPY Benchmark
strat.index <- index(Net.Strat)
Benchmark <- SPY.returns[index(Net.Strat)]
compare <- cbind(Net.Strat, Benchmark)
colnames(compare) <- c("Strategy (NET)", "SPY Benchmark")

#Performance
charts.PerformanceSummary(compare, main = "Out of Sample Strategy vs SPY Benchmark")
table.AnnualizedReturns(compare)
SortinoRatio(compare, MAR = 0)
maxDrawdown(compare[,1])

#Johansen Cointegration test
#test for robustness
jo.test <- ca.jo(Training.Clprices, type = "trace", ecdet = "none", K=2)
summary(jo.test)

#Signal Positions Plotted
z.score <- scale(Test.Resids)
zs.data <- coredata(z.score)
zs.high <- ifelse(zs.data > 1, zs.data, NA)
zs.low <- ifelse(zs.data < -1, zs.data, NA)

#Plot Polygon
plot(index(z.score), zs.data, type = "n",
     main = "Residual Spread",
     ylab = "Z-score",
     xlab = "Date")

#Poly for short signals (Red)
high.idx <- which(!is.na(zs.high))
polygon(c(index(z.score)[high.idx], rev(index(z.score)[high.idx])),
                c(zs.high[high.idx], rep(0, length(high.idx))),
                col = rgb(1, 0, 0, 0.3), border = NA)

#Poly for Long signals (Green)
low.idx <- which(!is.na(zs.low))
polygon(c(index(z.score)[low.idx], rev(index(z.score)[low.idx])),
        c(zs.low[low.idx], rep(0, length(low.idx))),
        col = rgb(0, 1, 0, 0.3), border = NA)

#Z-score lines
lines(index(z.score), zs.data, lwd = 1)

#Reference thresholds
abline(h = c(-1, 0 , 1), col = "grey", lty = 2)

#Legend
legend("topright",
       legend = c("Short Signal (z > 1)", "Long Signal (z < -1)"),
       fill = c(rgb(1, 0, 0, 0.3), rgb(0, 1, 0, 0.3)),
       bty = "n", cex = 0.8)

#Forecast
forecast <- ugarchforecast(fit, n.head = 5)
#conditional mean
plot(forecast, which = 1)
#conditional volatility
plot(forecast, which = 3)

#Estimated Volatility 
plot(fit@fit$sigma, type = 'l', main = "Estimated volatility for Net Strategy")

#Plot of closing prices
plot(AAPL$AAPL.Close, type ="l", main = "Closing Prices of AAPL and GOOG", col = "skyblue")
lines(GOOG$GOOG.Close, type = "l", col = "maroon")
addLegend("topleft", legend.names = c("AAPL", "GOOG"), col = c("skyblue", "maroon"), lty = 1, cex = 1)
  
