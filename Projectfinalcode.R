
#Setting working directory
setwd("C:\\Econ")

#Reading the dataset in csv format
a=read.csv("ECOMNSA.csv",col.names=c("DATE","econ"))
a

#Storing the time series data in a variable
s=ts(a$econ,start=c(1999,4),frequency=4)
s

#Plot of the time series
plot(s)

#Plot after taking log of the time series variable
plot(log(s))

#ACF of the time series variable
acf(s)

#PACF of the time series variable
pacf(s)
plot(log(s))

#Holding out the data starting from Q2 of 2014 
holdOut=ts(s,start=c(1999,4),end=c(2014,1),frequency=4)
holdOut

#Plot of holdout
plot(holdOut)

plot(log(holdOut))


#Unit root test for testing stationarity
library(urca)
testUR=ur.df(log(holdOut),lags=22,type="trend",selectlags="AIC")
summary(testUR)
plot(testUR)

#ACF of holdout
acf(log(holdOut))

#PACF of holdout
pacf(log(holdOut))


#MODEL1 using SARIMA
modelSARIMA100x111=Arima(log(holdOut),order=c(1,0,0),seasonal=c(1,1,1))
summary(modelSARIMA100x111)
acf(modelSARIMA100x111$residuals)

#MODEL2 using SARIMA
modelSARIMA101x111=arima(log(holdOut),order=c(1,0,1),seasonal=c(1,1,1))
summary(modelSARIMA101x111)
acf(modelSARIMA101x111$residuals)


#MODEL3 using SARIMA
modelSARIMA100x110=Arima(log(holdOut),order=c(1,0,0),seasonal=c(1,1,0))
summary(modelSARIMA100x110)
acf(modelSARIMA100x110$residuals)

#MODEL4 using SARIMA
modelSARIMA101x110=Arima(log(holdOut),order=c(1,0,1),seasonal=c(1,1,0))
summary(modelSARIMA101x110)
acf(modelSARIMA101x110$residuals)

#MODEL5 using SARIMA
modelSARIMA100x210=Arima(log(holdOut),order=c(1,0,0),seasonal=c(2,1,0))
summary(modelSARIMA100x210)
acf(modelSARIMA100x210$residuals)


#MODEL6 using SARIMA
modelSARIMA100x211=Arima(log(holdOut),order=c(1,0,0),seasonal=c(2,1,1))
summary(modelSARIMA100x211)
acf(modelSARIMA100x211$residuals)

#Forecast for SARIMA100x110
foreIN=forecast.Arima(modelSARIMA100x110,h=10,lambda = 0)
foreIN
foreIN$mean
foreIN$upper

#Upper forecast for SARIMA100x110
upper=ts(foreIN$upper[,2],start=c(2014,2),frequency=4)
upper

#Lower forecast for SARIMA100x110
lower=ts(foreIN$lower[,2],start=c(2014,2),frequency=4)
lower

plot(foreIN$mean,upper)
par(bg="cyan",font="2")


#Forecast plot for holdout
plot(cbind(window(s,start=c(2012,1)),foreIN$mean,upper,lower),main="Holdout Forecast",xlab="Years",ylab="Sales",plot.type="single",col=c("red","blue","black","black"),lwd=2,lty=c("solid","solid","dotted","dotted"))
legend("topleft",legend=c("Actual Sales","Forecasted Sales","Upper 95% CI","lower 95% CI"),fill=c("red","blue","black","black"))


#Best model with least AIC
forecastedmodelSARIMA100x110=Arima(log(s),order=c(1,0,0),seasonal=c(1,1,0))

#Summary of results 
summary(forecastedmodelSARIMA100x110)

#ACF of the best model chosen
acf(forecastedmodelSARIMA100x110$residuals)


#Forecast for the best model
foreOUT=forecast.Arima(forecastedmodelSARIMA100x110,h=2,lambda = 0)
foreOUT
foreOUT$mean
foreOUT$upper

#Upper forecast for the best model
upper=ts(foreOUT$upper[,2],start=c(2016,4),frequency=4)
upper

#Lower forecast for the best model
lower=ts(foreOUT$lower[,2],start=c(2016,4),frequency=4)
lower
plot(foreOUT$mean,upper)
par(bg="cyan",font="2")

#Ljung-Box test with 25 lags
Box.test(modelSARIMA100x110$residuals,lag=25,type="Ljung-Box")

#Plot of the forecasted model
plot(cbind(window(s,start=c(2012,1)),foreOUT$mean,upper,lower),main="2 Step Ahead Forecast",xlab="Years",ylab="Sales",plot.type="single",col=c("red","blue","black","black"),lwd=2,lty=c("solid","solid","dotted","dotted"))
legend("topleft",legend=c("Actual Sales","Forecasted Sales","Upper 95% CI","lower 95% CI"),fill=c("red","blue","black","black"))


