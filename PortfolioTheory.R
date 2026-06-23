
# compare stocks to bonds, correlation and portfolio theory
# relationship between value and growth over last few years
#pick an asset and project forward one year
#TLT BONDS

#Load Required Libraries 
install.packages("quantmod")     # For financial data
install.packages("PerformanceAnalytics")  # For risk & return metrics


library(quantmod)
library(PerformanceAnalytics)

#Importing Data 
getSymbols("TLT")
getSymbols("MSFT")
getSymbols("NVDA")
getSymbols("SPY")

#Ensuring data is loaded correctly
head(TLT)
tail(TLT)

#Calculate Daily Returns
mydata <- cbind(
  ROC(Cl(TLT)), 
  ROC(Cl(MSFT)), 
  ROC(Cl(NVDA)), 
  ROC(Cl(SPY))
)
colnames(mydata) <- c("TLTR", "MSFTR", "NVDAR", "SPYR")

#plot daily returns
plot(mydata, type = "l", main = "TLT, MSFT, NVDA, SPY returns", 
     xlab = "Date", ylab = "Price")
addLegend(legend.loc = 'bottom', inset = 0.02, c('TLT','MSFT','NVDA','SPY'), 
       col = c('red', 'black','green','blue'), lty = c(1,1))


#Performance Analysis 
charts.PerformanceSummary(mydata)
table.Stats(mydata)
table.DownsideRisk(mydata)
chart.Boxplot(mydata[, 1:4])
charts.RollingPerformance(mydata[, 1:4], width = 12)
addLegend(legend.loc = 'bottom', inset = 0.02, c('TLT','MSFT','NVDA','SPY'), 
          col = c('red', 'black','green','blue'), lty = c(1,1))

#Examination of Drawdowns for each individual asset
table.Drawdowns(mydata[,1]) #TLT
table.Drawdowns(mydata[,2]) #MSFT
table.Drawdowns(mydata[,3])  #NVDA
table.Drawdowns(mydata[,4])  #SPY

#Correlation Analysis
chart.Correlation(mydata)

#Rolling Correlation
chart.RollingCorrelation(Ra = ROC(Cl(SPY)), Rb = ROC(Cl(TLT)), width = 24,
                         main = "24-Day Rolling Correlation: SPY vs TLT")

#Plotting the Price Ratios of all assets
plot(Cl(TLT) / Cl(MSFT) / Cl(NVDA) / Cl(SPY), main = "Price Ratios")

#Comparison of SPY and TLT
getSymbols(c("SPY", "TLT"))  
mydata_spy_tlt <- cbind(
  ROC(Cl(SPY)), 
  ROC(Cl(TLT))
)
colnames(mydata_spy_tlt) <- c("SPYR", "TLTR")
charts.PerformanceSummary(mydata_spy_tlt)
table.Stats(mydata_spy_tlt)
table.DownsideRisk(mydata_spy_tlt)
table.Drawdowns(mydata_spy_tlt[, "SPYR"])
table.Drawdowns(mydata_spy_tlt[, "TLTR"])
chart.Correlation(mydata_spy_tlt)
chart.RollingCorrelation(Ra = mydata_spy_tlt[,"SPYR"], Rb = mydata_spy_tlt[,"TLTR"], width = 24)
plot(Cl(SPY)/Cl(TLT))

#CPI analysis and rolling correlation with SPY and TLT
getSymbols("CPIAUCSL", src = "FRED")

# Calculate the 12-month percentage change for CPI
CPI <- ROC(CPIAUCSL, 12, type = "discrete")
plot(CPI) #40 years of inflation stability, high inflation shock bad for bonds as interest rates go up, bad for stocks as high inflation and interest
CPIdf <- as.data.frame(CPI)
CPIdf$Date <- index(CPI)

# Calculate returns for SPY and TLT and combine
mydata2 <- cbind(
  ROC(Cl(SPY)), 
  ROC(Cl(TLT))
)
# Calculate the rolling correlation (using a 120-day window) between SPY and TLT returns
rlcorr <- rollapply(mydata2, width = 120, function(x) cor(x[, 1], x[, 2]), by.column = FALSE)
plot(rlcorr)
rlcorrdf <- as.data.frame(rlcorr)
rlcorrdf$Date <- index(rlcorr)

# Merge CPI data with the rolling correlation data
CPIcorrdata <- merge(CPIdf, rlcorrdf, by = "Date")
names(CPIcorrdata) <- c("Date", "CPI", "rlcorr")
plot(CPIcorrdata$Date, CPIcorrdata$CPI, type = "l", main = "CPI Over Time")

# A rebase function to normalize data
myrebase <- function(x, start = TRUE) {
  if(start == TRUE){
    x / x[1] * 100
  } else {
    x / x[length(x)] * 100
  }
}
#Repeat NA's up to 4th row rebase begins on 5th row and ignore NA
CPIcorrdata$CPIrb <- c(rep(NA, 4), myrebase(CPIcorrdata$CPI[-c(1:4)]))
CPIcorrdata$rlcorrb <- c(rep(NA, 4), myrebase(CPIcorrdata$rlcorr[-c(1:4)]))

# Plot normalized CPI and rolling correlation together
plot(CPIcorrdata$Date, CPIcorrdata$CPIrb, type = "l", ylim = c(-200, 500), 
     main = "Correlation of Stocks and Bonds vs Inflation")
lines(CPIcorrdata$Date, CPIcorrdata$rlcorrb, col = "blue", lty = 2)
graphics::legend("bottom", inset = 0.05, cex = 0.8,
                 legend = c("Inflation", "Correlation of stocks and bonds"), 
                 lty = c(1, 2), col = c("black", "blue"))
abline(h = 100, col = "red")

#Correlation going down as inverted, inflation going towards 2% target 

# Linear models to examine the relationship between inflation and the rolling correlation
eq1 <- lm(rlcorrb ~ CPIrb, data = CPIcorrdata)#low r2 suggest more variables at play
summary(eq1)
eq2 <- lm(rlcorr ~ CPI, data = CPIcorrdata) #Cpi significantly affects correlation, r2 is low suggesting more at play
summary(eq2)

#Future Projection: 
#Non-parametric simulation, Creating Matrix, 252 rows for each trading day, 1000 columns fro amount of simulations to ensure robust sample.
siNPmatrix <- matrix(NA, nrow = 252, ncol = 1000)
#as data frame to convert xts, 
NVDAdf<-as.data.frame(NVDA)

#confirming closing prices
head(NVDAdf$NVDA.Close)

#sets the first row of every simulation equal to the most recent closing price
siNPmatrix[1, ] <- tail(NVDAdf$NVDA.Close, 1)

#computes the log prices which are more suitable for returns
NVDAsimR <- diff(log(NVDAdf$NVDA.Close))

#randomly samples 251 log returns and replaces them from historical returns exluding the first one
siNPmatrix[1, ] <- NVDAdf$NVDA.Close[length(NVDAdf$NVDA.Close)]
for(i in 1:1000){
  siNPmatrix[-1, i] <- siNPmatrix[1, i] * cumprod(1 + sample(NVDAsimR [-1], 
                                                             251, 
                                                             replace = TRUE))
}
  #Projecting Forward,  adds first path then remaining 999 paths to show range of possible prices 
  plot(siNPmatrix[, 1], type = 'l', 
       ylim = c(min(siNPmatrix), max(siNPmatrix)), 
       main = "NVDA NP Simulation", lwd = 4)
  for(i in 2:1000){
    lines(siNPmatrix[, i], col = rgb(0.01, 0, 0, 0.3), lwd = 1)
        
          
  }
  # provides average simulated price for 5th percentile up to 95th to quantify uncertainty for future prices
  mean(siNPmatrix)
  quantile(siNPmatrix, 0.05)
  quantile(siNPmatrix, 0.20)
  quantile(siNPmatrix, 0.80)
  quantile(siNPmatrix, 0.95)
  # links to theory by not assuming normal distributions for returns which has fat tail. 
  
  