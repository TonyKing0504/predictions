library(knitr)
library(zoo)
library(tseries)
library(forecast)
library(tidyr)

nino_data <- read.csv("NINO34.csv") 
nino_vector <- tidyr::pivot_longer(Jan:Dec, names_to = "month", values_to = "Nino34", data = nino_data)

nino_ts <- ts(nino_vector$Nino34, start=c(1870, 1), end = c(2023,11), frequency=12)

# Split the data into training and test sets
train1 <- window(nino_ts, end=c(2021, 12))
test1 <- window(nino_ts, start=c(2022, 1), end=c(2023, 11))

# plot train1 data and its acf and pacf
plot(train1)
acf(coredata(train1))
pacf(coredata(train1))

decomposed <- stl(train1, s.window = "periodic")
remainder <- train1 - decomposed$time.series[, 1]
plot(remainder)
acf(remainder)
pacf(remainder)

AR1 <- arima(remainder, order=c(1,0,0)) 
AR6 <- arima(remainder, order=c(6,0,0))

AIC(AR1)
AIC(AR6)

tsdiag(AR1)
tsdiag(AR6)

s_value <- decomposed$time.series[1:12, 1]
p_ar1 <- predict(AR1, newdata = test1, n.ahead = 23, level = 0.95, prediction.interval = T)$pred + s_value

p_ar6 <- predict(AR6, newdata = test1, n.ahead = 23, level = 0.95, prediction.interval = T)$pred + s_value

mspe_ar1 <- mean((test1 - p_ar1)^2)
mspe_ar6 <- mean((test1 - p_ar6)^2)
mspe_ar1
mspe_ar6

se_ar1 <- predict(AR1, newdata = test1, n.ahead = 23, level = 0.95, prediction.interval = T)$se
se_ar6 <- predict(AR6, newdata = test1, n.ahead = 23, level = 0.95, prediction.interval = T)$se
plot(test1, lty = 1, col = 1, ylim = c(24, 30))
lines(p_ar1, lty = 1, col = 2)
lines(p_ar6, lty = 1, col = 3)
lines(p_ar1 - 1.96 * se_ar1, lty = 2, col = 2)
lines(p_ar1 + 1.96 * se_ar1, lty = 2, col = 2)
lines(p_ar6 - 1.96 * se_ar6, lty = 2, col = 3)
lines(p_ar6 + 1.96 * se_ar6, lty = 2, col = 3)

legend("topleft", lty = 1, col = 1:3, legend = c("test", "ar1", "ar6"))

hw_model <- HoltWinters(train1, beta = T, gamma = T)
prediction_hw <- predict(hw_model, newdata = test1, n.ahead = 23, prediction.interval = T)[,1]
prediction_lwr <- predict(hw_model, newdata = test1, n.ahead = 23, prediction.interval = T)[,3]
prediction_upper <- predict(hw_model, newdata = test1, n.ahead = 23, prediction.interval = T)[,2]

mspe_hw <- mean((test1 - prediction_hw) ^ 2)
mspe_hw

plot(test1, lty = 1, col = 1, ylim = c(2, 30))
lines(p_ar6, lty = 1, col = 3)
lines(prediction_hw, lty = 1, col = 4)
lines(p_ar6 - 1.96 * se_ar6, lty = 2, col = 3)
lines(p_ar6 + 1.96 * se_ar6, lty = 2, col = 3)
lines(prediction_lwr, lty = 2, col = 4)
lines(prediction_upper, lty = 2, col = 4)

legend("topleft", lty = 1, col = 1:4, legend = c("test","ar6","HoltWinter"))

