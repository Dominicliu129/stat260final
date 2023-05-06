
library(tseries)
library(forecast)

# 1. Rainfall data

df <- read.csv("/Users/shahsiangchuan/Downloads/CSVDataServlet.txt")
df <- df[,c('DATE.TIME','VALUE')] 

## Create a new column for date.
df$DATE <- substr(df$DATE.TIME,1,8)

## Group the hourly date with daily mean.
TLD <- aggregate(VALUE ~ DATE, data = df, mean)


## Transform the data into time-series object.

TLD_ts <- ts(TLD['VALUE'], start=c(2018,5), end=c(2023,5), frequency = 365)
TLD_ts <- TLD_ts[1:1797]

plot(TLD$VALUE,type='l')


## Split the data with training and testing sets.

TLD_train <- TLD_ts[-1:-30]
TLD_test <- rev(rev(TLD_ts)[1:30])

## Train the data with ARIMA model.
TLD_arima <- auto.arima(TLD_train, seasonal=T)


## Do the forecast on the testing set and calculate the MSE.

TLD_forecast <- forecast(TLD_train, length(TLD_test))
TLD_mse <- mean((TLD_test-TLD_forecast$mean)^2)
TLD_mse



# 2. S&P 500

stock_df <- read.csv('/Users/shahsiangchuan/Downloads/HistoricalData_1683073126605.csv')
stock <- stock_df[,c('Close.Last')]
stock <- rev(stock)

## Transform the data into time-series object.

stock_ts <- ts(stock)


## Normalized the data.

stock_ts_norm <- (stock_ts - mean(stock_ts))/sd(stock_ts)
plot(stock_ts,type='l')


## Split the data with training and testing sets.

stock_train <- stock_ts_norm[-1:-20]
stock_test <- rev(rev(stock_ts_norm)[1:20])


## Train the data with ARIMA model.

stock_arima <- auto.arima(stock_train, seasonal=F)


## Do the forecast on the testing set and calculate the MSE.

stock_forecast <- forecast(stock_train, length(stock_test))
stock_mse <- mean((stock_test-stock_forecast$mean)^2)


## Transform the MSE with to original scale.

stock_mse <- stock_mse * sd(stock_ts) + mean(stock_ts)
stock_mse


## Transform the predicted value to the original scale.

stock_forecast_unnorm <- stock_forecast$mean * sd(stock_ts) + mean(stock_ts)
stock_forecast_unnorm

plot(stock_forecast)



# 3. Google Trend

google <- read.csv('/Users/shahsiangchuan/Downloads/google_trend_ai.csv',header=F)
google <- google[3:262,2]
google <- as.numeric(google)

plot(google,type='l')

## Transform the data into time-series object.

google_ts <- ts(google)


## Split the data with training and testing sets.

google_train <- google_ts[-1:-5]
google_test <- rev(rev(google_ts)[1:5])


## Train the data with ARIMA model.

google_arima <- auto.arima(google_train, seasonal=F)


## Do the forecast on the testing set and calculate the MSE.

google_forcast <- forecast(google_arima, length(google_test))
google_mse <- mean((google_forcast$mean - google_test)^2)
google_mse

plot(google_forcast)



