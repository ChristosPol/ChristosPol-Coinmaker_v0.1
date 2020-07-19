# Daily data closing prices
View(candles)
library("fpp2")
library("ggplot2")
library(forecast)
rm(list=ls())

candles_Test <- candles[(nrow(candles)-20) : nrow(candles), ]
candles_Train <- candles[1: (nrow(candles)-20) , ]

Y <- ts(candles_Train$close, start = c(2019,1, 1), frequency = 365)

plot(myts)

inds <- seq(as.Date(head(candles$Date,1)), as.Date(tail(candles$Date,1)), by = "day")

## Create a time series object
set.seed(25)
myts <- ts(candles_Train$close,     # random data
           start = c(2019, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
acf(myts) # give MA terms
pacf(myts) # AR terms
auto.arima(myts)
fit <- arima(myts, order = c(1,1,1))
fit_resid <- residuals(fit)
Box.test(fit_resid, lag =3, type ="Ljung-Box")
forecast_myts <- forecast(fit, h = 20)
cbind(candles_Test, forecast = as.vector(forecast_myts$mean))
candles_Test



autoplot(myts)
di <- diff(myts)
autoplot(di)


ggseasonplot(di)

fit <- snaive(di)
summary(fit)
checkresiduals(fit)


fit <- ets(di)
fit <- auto.arima(myts,d=1, D =1, stepwise = FALSE, approximation = TRUE, trace = T)
autoplot(forecast(fit, h = 5))
df <-cbind(candles_Test, forecast = as.vector(forecast(fit, h = 20)$mean))

lines(df$close, type ="l")
plot(df$forecast, type ="l")
