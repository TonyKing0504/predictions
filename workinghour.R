library(knitr)
library(zoo)
library(tseries)
library(forecast)
library(tidyr)

dat <- read.csv("usual_hours_worked_ca.csv")
dat_ts <- ts(dat$Hours, start = c(1987,1), end = c(2023, 12), frequency = 12)
train2 <- window(dat_ts, start = c(1987, 1), end = c(2020, 12))
test2 <- window(dat_ts, start = c(2021, 1), end = c(2023, 12))
plot(train2)

diff_train <- diff(train2, differences = 1)
plot(diff_train)
acf(diff_train)
pacf(diff_train)

sdif_train <- diff(diff_train, lag = 12)
plot(sdif_train)
acf(sdif_train)
pacf(sdif_train)

aic_mat <- matrix(nrow=5, ncol=5)

for(q in 0:4){
  for(Q in 0:4){
    model <- arima(train2, order = c(2,1,q),
                   seasonal = list(order = c(1,1,Q),period = 12))
    aic_mat[q+1,Q+1] <- AIC(model)
  }
}

aic_mat == min(aic_mat, na.rm = T)

final_fit <- arima(train2, order = c(2,1,2),
                   seasonal = list(order = c(1,1,4), period = 12))

tsdiag(final_fit)

prediction_arima <- predict(final_fit, newdata = test2, n.ahead = 23)$pred
prediction_arima_se <- predict(final_fit, newdata = test2, n.ahead = 23)$se

plot(test2, lty = 1, col = 1, ylim = c(34, 37))
lines(prediction_arima, lty = 1, col = 5)
lines(prediction_arima - 1.96 * prediction_arima_se, lty = 2, col = 5)
lines(prediction_arima + 1.96 * prediction_arima_se, lty = 2, col = 5)

legend("topleft", lty = 1, col = c(1, 5), legend = c("test","arima"))

hw_model2 <- HoltWinters(train2, beta = T, gamma = T)
hw_model2$coefficients

prediction_hw2 <- predict(hw_model2, newdata = test2, n.ahead = length(test2), prediction.interval = T)[,1]
prediction_lwr2 <- predict(hw_model2, newdata = test2, n.ahead = length(test2), prediction.interval = T)[,3]
prediction_upper2 <- predict(hw_model2, newdata = test2, n.ahead = length(test2), prediction.interval = T)[,2]

plot(test2, lty = 1, col = 1, ylim = c(20, 55))
lines(prediction_hw, lty = 1, col = 4)
lines(prediction_lwr, lty = 2, col = 4)
lines(prediction_upper, lty = 2, col = 4)
legend("topleft", lty = 1, col = c(1, 4), legend = c("test","HoltWinter") )

mspe_arima <- mean((test2 - prediction_arima)^2)
mspe_arima
mspe_hw2 <- mean((test2 - prediction_hw2)^2)
mspe_hw2