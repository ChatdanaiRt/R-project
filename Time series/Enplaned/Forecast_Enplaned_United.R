library(forecast)

data = Forecast_baggagecomplaints

y = ts(data[,2],start = c(2004,1),frequency = 12)

#chek stationary by graph
plot(y,main="Plot:Enplaned Data",ylab="Enplaned")

#Tranformation data to stationary data by diff data
#chek stationary by graph
dy = diff(y)
plot(dy,main="Plot:Enplaned Data",ylab="Enplaned")

#investigate seasonality
ggseasonplot(dy,main = "Seasonal plot : Change in Enplaned",ylab="Enplaned")

#subseries plot
ggsubseriesplot(dy)

#compare method
#forecast various method

#seasonal native method
fnaive = snaive(dy)
print(summary(fnaive))
checkresiduals(fnaive)
#Residual sd : 181416.0108, MAPE = 136.7788

#arima method
#inding possible values for d and D.
model1 <- auto.arima(y, seasonal = TRUE)
summary(model1)
tsdiag(model1)
#d = 1, D = 1

#finding the best arima model
farima = auto.arima(y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE)
print(summary(farima))
checkresiduals(farima)
resifarima <- residuals(farima)
resifarima_sd <- sd(resifarima)
#residual sd = 107923
#MAPE = 1.724646 ,d=1, D=1


#Forecast model arima
fc = forecast(farima,h=24)
plot(fc)
print(summary(fc))
