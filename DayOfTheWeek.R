# Market Efficiency Analysis: Testing the Day-of-the-Week Anomaly on the FTSE 100

## Project Overview
This repository hosts an empirical financial econometrics study written in R that investigates a classic market anomaly: the **Day-of-the-Week Effect**. Using historical data from the **FTSE 100 Index**, the research tests the Weak-Form Efficient Market Hypothesis (EMH) to determine if structural return variations exist across specific weekdays.

The analytical pipeline is broken into three systematic stages:
1. **Distributional Modeling:** Isolating return metrics by individual days and testing for normality, skewness, and fat-tailed distributions.
2. **Analysis of Variance (ANOVA) & OLS Modeling:** Testing the statistical significance of daily mean differentials under robust standard error controls.
3. **Serial Correlation Adjustments:** Resolving residual time-dependency via Ljung-Box diagnostic loops and an Autoregressive [AR(p)] specification.

---

## Technical Framework & Methodology

### 1. Statistical Moments & Normality Controls
Before building parametric models, the script breaks down daily log-returns into conditional subsets based on the day of the week:
* **The Descriptive Matrix:** The `stargazer` package formats baseline summary statistics (mean, median, standard deviation, and range limits) for each unique weekday vector.
* **Higher-Order Moments:** The returns are evaluated for Skewness and Kurtosis to isolate non-normal asymmetric features.
* **The Jarque-Bera Test:** A formalized Jarque-Bera test is executed on the composite return profile to mathematically prove that stock index innovations exhibit significant fat-tails ($p < 0.05$), validating the necessity of robust regression approaches.

### 2. Hypothesis Testing & Structural Anomalies
To determine if average returns systematically differ between trading days, the script executes two parallel statistical frameworks:
* **ANOVA Specification:** An Analysis of Variance (`aov`) framework tests the global null hypothesis that the mean log-returns across all five weekdays are mathematically identical ($H_0: \mu_{\text{Mon}} = \mu_{\text{Tue}} = \dots = \mu_{\text{Fri}}$).
* **Dummy-Variable Linear Model:** An OLS regression maps the return array against weekday factors. To control for potential heteroskedasticity in daily return variances, the script applies White-corrected standard errors via Heteroskedasticity-Consistent (`HC1`) covariance estimations.

### 3. Time-Dependency Diagnostics & Autoregressive Correction
A common failure in market anomaly research is ignoring serial correlation (where yesterday's return impacts today's return), which inflates t-statistics and leads to false conclusions. This framework implements a structural diagnostic cycle:
* **Autocorrelation Analysis:** The Sample Autocorrelation Function (`acf`) evaluates the model's residuals for lingering serial patterns.
* **Automated Ljung-Box Loops:** The code loops through a series of lags to execute consecutive Ljung-Box independence tests (`Box.test`), mapping p-values against a $5\%$ significance boundary line to pinpoint structural serial dependency.
* **Akaike Information Criterion (AIC) Grid Search:** To determine the correct lag depth, an automated loop fits consecutive Autoregressive models up to an $\text{AR}(15)$ order, isolating the absolute minimum AIC value to find the optimal lag length.
* **The Autoregressive Return Specification:** The final refined model integrates lagged return variables alongside weekday factor variables to cleanly isolate the true weekday anomaly effects from simple momentum feedback loops:
$$\text{Return}_t = \beta_0 + \sum_{i=1}^{4} \gamma_i \text{Return}_{t-i} + \sum_{j=1}^{4} \delta_j \text{DayDummy}_{j,t} + \epsilon_t$$

---

## Technical Ecosystem & Dependencies
* **Data Retrieval & Time-Series Engineering:** `quantmod`, `xts`, `zoo`
* **Distributional Dynamics & Testing:** `moments`, `tseries`
* **Linear Modeling & Econometric Analytics:** `dplyr`, `parameters`, `stargazer`
* **Data Visualization & Plotting Suites:** `ggplot2`

When testing for market anomalies like the Day-of-the-Week effect, your residuals will often exhibit serial correlation. If you ignore this, your standard error estimates are biased, which can make a random return patterns look statistically significant. I solved this by running an AIC grid search to identify the optimal lag structure and then fitting an Autoregressive [AR(p)] model to filter out price momentum before testing the weekday coefficients.

The Jarque-Bera test mathematically confirms that stock index returns reject normality due to extreme kurtosis. This is a crucial concept when evaluating risk, because assuming a normal distribution severely understates the frequency of tail-risk events and market shocks.

--------------------------------------------------

library(quantmod)
library(forecast)
library(dplyr)
library(ggplot2)
#install.packages("stargazer")
library(stargazer)
library(moments)
library(tseries)
library(parameters)



getSymbols("^FTSE", from="2016-01-01", to="2020-02-29", auto.assign = TRUE)
View(FTSE)

date<-index(FTSE)
FTSER<-dailyReturn(FTSE, type = "log")*100

date<-index(FTSER)
dayoftheweek<-weekdays(date)

length(FTSER)

#day of the week effect

df<-data.frame(date=date,FTSER=as.numeric(FTSER), dayoftheweek)

stargazer(subset(df, select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(df$FTSER)
kurtosis(df$FTSER)
jarque.bera.test(df$FTSER)

stargazer(subset(df, dayoftheweek=="Monday" , select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(subset(df, dayoftheweek=="Monday" , select=c(FTSER)))
kurtosis(subset(df, dayoftheweek=="Monday" , select=c(FTSER)))

stargazer(subset(df, dayoftheweek=="Tuesday" , select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(subset(df, dayoftheweek=="Tuesday" , select=c(FTSER)))
kurtosis(subset(df, dayoftheweek=="Tuesday" , select=c(FTSER)))

stargazer(subset(df, dayoftheweek=="Wednesday" , select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(subset(df, dayoftheweek=="Wednesday" , select=c(FTSER)))
kurtosis(subset(df, dayoftheweek=="Wednesday" , select=c(FTSER)))

stargazer(subset(df, dayoftheweek=="Thursday" , select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(subset(df, dayoftheweek=="Thursday" , select=c(FTSER)))
kurtosis(subset(df, dayoftheweek=="Thursday" , select=c(FTSER)))

stargazer(subset(df, dayoftheweek=="Friday" , select=c(FTSER)), type = "text", summary.stat = c("mean", "median", "sd", "min", "max") )
skewness(subset(df, dayoftheweek=="Friday" , select=c(FTSER)))
kurtosis(subset(df, dayoftheweek=="Friday" , select=c(FTSER)))

average_return<-aggregate(FTSER*100, by=list(df$dayoftheweek), FUN = mean)
average_return

as.numeric(average_return[5])-as.numeric(average_return[2])

average_return<-as.data.frame(average_return)
average_return$weekdays<-rownames(average_return)
colnames(average_return)<-c("AVG_Return","weekdays")

ggplot(average_return, aes(x=weekdays, y=AVG_Return, fill=weekdays))+
  geom_bar(stat="identity", color="black")+
  theme_minimal()+
  labs(title="Average Return by Day of the Week",x="Day of the Week",y="Average log return")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

result=aov(FTSER~dayoftheweek, data = df)
summary(result)

df$dayoftheweek<-as.factor(df$dayoftheweek)
model1<-lm(FTSER~dayoftheweek, data = df)
summary(model1)

parameters(model1, vcov = "HC1")

plot(model1$residuals, type = "l")

acf(model1$residuals)

lags<-1:15
pvalues<-c()

for (i in lags) {
  
  y<-Box.test(model1$residuals, lag = 2, type ="Ljung-Box")
  pvalues<-c(pvalues,y$p.value)
  
}

plot(lags, pvalues, lwd=3, ylim = c(0,1), xlab = "Lags", main = "Ljung-Box")
abline(h=0.05, col ="blue", lty="dashed")

aic<-c()
for (i in 1:15) {
  modelin<-arima(FTSER, order=c(i,0,0))
  z<-AIC(modelin)
  aic<-c(aic,z)
}

aic

min(aic)


df2<-data.frame(date=date,FTSER=as.numeric(FTSER), dayoftheweek)

model2<-lm(FTSER~lag(FTSER,1)+lag(FTSER,2)+lag(FTSER,3)+lag(FTSER,4)+dayoftheweek, data=df)
summary(model2)

parameters(model2, vcov = "HC1")

acf(model2$residuals, lag=15)
lags<-1:15
pvalues<-c()

for (i in lags) {
  
  y<-Box.test(model1$residuals, lag = 1, type ="Ljung-Box")
  pvalues<-c(pvalues,y$p.value)
  
}

plot(lags, pvalues, lwd=3, ylim = c(0,1), xlab = "Lags", main = "Ljung-Box")
abline(h=0.05, col ="blue", lty="dashed")

