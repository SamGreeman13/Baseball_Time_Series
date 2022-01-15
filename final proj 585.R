# R code used

library(readr)
library(tidyverse)
library(forecast)
library(itsmr)
library(aTSA)
library(EnvStats)
library(tseries)

cin <- read.csv("C:/Users/Sam/Desktop/wcin.csv")

cinc <- cin$Wins
plot.ts(cinc, ylab="Winning Percentage", xlab="Years Since 1947", main="Cinncinnati Reds Yearly Win Percentage")
adf.test(cinc)
diff_cin <- ts(diff(cinc, differences = 1))
adf.test(diff_cin)
plot(diff_cin, ylab="Differenced Winning Percentage", xlab="Years Since 1947", main="Differenced Yearly Win Percentage")
acf(cinc, main="ACF Plot for Reds Winning Percentage")
pacf(cinc, ylim=range(-0.25, 0.5), main="Partial ACF Plot for Reds Winning Percentage")
acf(diff_cin, main="ACF Plot for Differenced Reds Winning Percentage")
pacf(diff_cin, ylim=range(-0.5, 0.25), main="Partial ACF Plot for Differenced Reds Winning Percentage")
#AR(2), (0.3465, 0.23) or (0.335, 0.2442)
#ARIMA(0,1,1), (-0.661) or (-0.6604)

cin_es <- ses(cinc,alpha=0.6,initial="simple")
plot(cinc)
plot(fitted(cin_es), ylab="Smoothed Winning Percentage", xlab="Years Since 1947", main="Exponential Smoothing of Reds Data")
lines(cinc, col="red")
adf.test(fitted(cin_es))

cin_esd <- ses(diff_cin,alpha=0.6,initial="simple")
adf.test(fitted(cin_esd))

cinc_arima <- arima(cinc, order=c(0,1,1))
forecast::forecast(cinc_arima, h=5)
hw_cin <- HoltWinters(cinc, gamma=F)
forecast::forecast(hw_cin, h=5)
cinc_es <- ses(cinc,alpha=0.6,initial="simple")
forecast::forecast(cinc_es, h=5)

train_cinc <- head(cinc, n=66)
test_cin <- tail(cinc, n=7)
cinc_val_hw <- HoltWinters(train_cinc, gamma=F)
fore_1 <- forecast::forecast(cinc_val_hw, h=7)
fore_1
error_hw <- test_cin - fore_1$mean
rmse_hw <- sqrt(abs(mean(error_hw)))
mae_hw <- mean(abs(error_hw))
mape_hw <- mean(abs((error_hw*100)/test_cin))

cinc_val_arima <- arima(train_cinc, order=c(0, 1, 1))
fore_2 <- forecast::forecast(cinc_val_arima, h=7)
fore_2
error_arima <- test_cin - fore_2$mean
rmse_arima <- sqrt(abs(mean(error_arima)))
mae_arima <- mean(abs(error_arima))
mape_arima <- mean(abs((error_arima*100)/test_cin))

cinc_val_es <- ses(train_cinc,alpha=0.6,initial="simple")
cinc_val_esd <- arima(fitted(cinc_val_es), order=c(0,1,1))
fore_4 <- forecast::forecast(cinc_val_esd, h=7)
fore_4
error_esd <- test_cin - fore_4$mean
rmse_esd <- sqrt(abs(mean(error_esd)))
mae_esd <- mean(abs(error_esd))
mape_esd <- mean(abs((error_esd*100)/test_cin))

cinc_val_es <- ses(train_cinc,alpha=0.6,initial="simple")
fore_3 <- forecast::forecast(cinc_val_es, h=7)
fore_3
error_es <- test_cin - fore_3$mean
rmse_es <- sqrt(abs(mean(error_es)))
mae_es <- mean(abs(error_es))
mape_es <- mean(abs((error_es*100)/test_cin))

arima(cinc, order=c(2,1,1))
arima(cinc, order=c(2,0,0))
arima(cinc, order=c(1,1,0))
arima(cinc, order=c(0,1,1))
arima(cinc, order=c(0,1,2))
arima(cinc, order=c(1,1,1))


acf(diff_cin)
pacf(diff_cin)




##-0.46



rmse_hw
rmse_arima
rmse_es
rmse_esd

mae_hw
mae_arima
mae_es
mae_esd

mape_hw
mape_arima
mape_es
mape_esd

plot(fore_1)
plot(fore_2)
plot(fore_3)
