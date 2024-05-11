library(forecast)
library(tseries)
library(ggplot2)

# getwd()
# list.files()

data <- read.table("JJ.dat", header = TRUE)

# print(dim(data))
# print(names(data))

ts_data <- ts(data$JJ, start=c(1960,1), frequency=4)
plot(ts_data, xlab="Year", ylab="Earnings", main="Quarterly Earnings per Share for Johnson & Johnson")
decomposed <- stl(ts_data, s.window="periodic")
plot(decomposed)

# check stationary w/ adf
adf.test(ts_data, alternative = "stationary")

# # fit with auto ARIMA
# initial_fit <- auto.arima(ts_data)
# summary(initial_fit)
# checkresiduals(initial_fit)

# first order differencing
ts_data_diff <- diff(ts_data, differences = 1)
plot(ts_data_diff, main="Differenced Time Series", xlab="Time", ylab="Differenced Earnings")
adf.test(ts_data_diff)

acf(ts_data_diff, main="ACF of Differenced Series")
pacf(ts_data_diff, main="PACF of Differenced Series")

# fit
fit <- arima(ts_data, order=c(4,1,1), method="ML")
summary(fit)
checkresiduals(fit)

# forecast
forecast <- forecast(fit, h=12)
plot(forecast)


