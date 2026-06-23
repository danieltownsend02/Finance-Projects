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

