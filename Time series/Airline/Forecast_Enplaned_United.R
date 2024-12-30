library(forecast)
library(tseries)
library(nortest)

data <- read_excel("Airline.xlsx")
data <- subset(data, Airline == "United")
data <- data[,c(2,8)]

y <- ts(data[,2],start = c(2004,1),frequency = 12)

#chek stationary by graph
plot(y,main="Plot:Enplaned Data",ylab="Enplaned")

#investigate seasonality
ggseasonplot(dy,main = "Seasonal plot : Change in Enplaned",ylab="Enplaned")


##compare method
#forecast various method

#seasonal native method
fnative <- snative(dy)
print(summary(fnaive))
checkresiduals(fnaive)
### ** Residual sd : 181416.0108, MAPE = 136.7788 **


#finding the best ARIMA model
arima1 <- auto.arima(y,stepwise = FALSE,trace = TRUE,seasonal = TRUE)
print(summary(arima1))
checkresiduals(arima1)
# ** autocorrelation in residuals ** Try to find new ARIMA model

arima2 <- auto.arima(y,d=1,D=0,stepwise = FALSE,trace = TRUE,seasonal = TRUE)
print(summary(arima2))
resiarima <- residuals(arima2)

checkresiduals(arima2)
# ** No - autocorrelation in residuals **
ad.test(resiarima)
# ** normality in residuals **

resiarima_sd <- sd(resiarima)
### ** Residual sd = 210824.4,#MAPE = 3.482151 **, d=1, D=0


#Forecast 2 years by ARIMA model
fc <- forecast(arima2,h=24)
plot(fc)
print(summary(fc))
