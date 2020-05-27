install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")
install.packages("stR")
library(ggplot2)
library(forecast)
library(tseries)
library(stR)

setwd("D:\\Data Science\\DSA\\7.Forecasting\\Forecasting_Handson")


################################# Seasonal Decomposition for Euro Data ##########################################################
euro <-  read.csv("euro.csv", header = FALSE)
eurotail <- ts(euro, frequency = 4)

#Frequency is the function heps us to divide the data into sequence we need (Weekly(7), Monthly(12), quarterly(4))
#ts is the time series helps us to divide the given data into a time series with frequency.

myts<- ts(euro$V1, frequency = 4)
myts
plot(myts, col="gray",
     main="Euro Retail",
     ylab="Index", xlab="")


#Decompose Timeseries, stl is Decompose a time series into seasonal, trend and irregular components using loess.

fit <- stl(myts, s.window=4)

#Functions to check if an object is a data frame, or coerce it if possible.
timesplit <- as.data.frame(fit$time.series)


#Plot Trend component
plot(myts, col="gray",
     main="Euro Retail",
     ylab="Index", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")


#Plot all the time series components together
plot(fit)

#Plot Monthly trend(Here in our case it is Q1, Q2,Q3,Q4)
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

################################# Moving Averages smoothing for Euro Data ##########################################################

#ma computes a simple moving average smoother of a given time series, It means it creates an average of every 5 data points and creates a new data point.
fitma <- ma(euro, order=5)

#Plot Actual vs forecast
plot(euro[c(6:64),], col="gray", type = "l",
     main="Euro Retail",
     ylab="Index", xlab="")
lines(fitma,col="red",ylab="Trend")

fcma<- forecast(fitma, h=12)# h =12 is Number of periods for forecasting
Valueforecastma <- as.data.frame(cbind(fcma$mean, fcma$lower, fcma$upper))

################################# Simple exponential smoothing for Euro Data ##########################################################


# Simple exponential smoothing- Holtwinters funtion
# h is the no. periods to forecast
fitexp <- ses(euro$V1, alpha= 0.2, initial="simple")
fitexp2 <-ses(euro$V1, initial= c("optimal","simple"), alpha = NULL, h = 12)
obsexp <- as.data.frame(cbind(fitexp$fitted,fitexp$residuals))

alphaop <- fitexp2$model$par
alphaop

accuracy(fitexp)
accuracy(fitexp2)


#plot with alpha=.2
plot(euro$V1, col="gray", type = "l",
     main="Euro Retail",
     ylab="Index", xlab="")
lines(fitexp$fitted,col="red",ylab="Trend")


#plot with alpha=optimal
plot(euro$V1, col="gray", type = "l",
     main="Euro Retail",
     ylab="Index", xlab="")
lines(fitexp2$fitted,col="red",ylab="Trend")


Valueforecastexp <- as.data.frame(cbind(fitexp2$mean, fitexp2$lower, fitexp2$upper))



################################# ARIMA Model for Euro Data ##########################################################


#Plot original series
plot(eurotail, ylab="Retail index", xlab="Year")



#Check stationarity, it indiactes Not stationary
adf.test(eurotail) # p-value < 0.05 indicates the TS is stationary


#Plot ACF and PACF, Higher order PACF Lags#4 suggests seasonal effects  
acf(eurotail)
pacf(eurotail)

# Take first seasonal difference with 4 as quarters
euroSdif <- diff(eurotail,4)
tsdisplay(euroSdif)
adf.test(euroSdif) # p-value < 0.05 indicates the TS is stationary


# Higher order ACF suggests non seasonal differencing, So Take first Non-seasonal difference and see the series is now stationary
euroNSdif <- diff(euroSdif)
tsdisplay(euroNSdif)
adf.test(euroNSdif) # p-value < 0.05 indicates the TS is stationary

# Fitting AR and MA terms

fitcheck <- Arima(eurotail, order=c(0,1,0), seasonal=c(0,1,0))
tsdisplay(residuals(fitcheck))

#The significant spike at lag 1 in the ACF suggests a non-seasonal AR(1) component, and 
#the significant spike at lag 4 in the ACF and PACF suggests a seasonal MA(1) component


fitARIMA <- Arima(eurotail, order=c(1,1,2), seasonal=c(0,1,1))
tsdisplay(residuals(fitARIMA))



#Plot forecasts 80% and 95% prediction intervals are shown
plot(forecast(fitARIMA, h=12))

fc<- forecast(fitARIMA, h=12)

Valueforecast <- as.data.frame(cbind(fc$mean, fc$lower, fc$upper))

#Plot actual Vs fitted
plot(myts, col="gray", type = "l",
     main="Euro Retail",
     ylab="Index", xlab="")
lines(fitARIMA$fitted,col="red",ylab="Trend")

accuracy(fitARIMA)

