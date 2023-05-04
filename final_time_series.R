
library(tseries)
library(quantmod)
library(forecast)
df <- read.csv("/Users/shahsiangchuan/Downloads/CSVDataServlet.txt")
df <- df[,c('DATE.TIME','VALUE')] 

df$DATE <- substr(df$DATE.TIME,1,8)

TLD <- aggregate(VALUE ~ DATE, data = df, mean)
TLD_ts <- ts(TLD['VALUE'], start=2018, end=2023, frequency = 365)

plot(TLD_ts)

TLD_decompose <- decompose(TLD_ts)
plot(TLD_decompose)
TLD_adjusted = TLD_ts - TLD_decompose$seasonal
plot(TLD_adjusted)


TLD_train <- TLD_ts[-1:-30]
TLD_test <- rev(TLD_ts)[1:30]

## With seasonality
TLD_arima <- auto.arima(TLD_train, seasonal=T)
TLD_forecast <- forecast(TLD_train, length(TLD_test))
TLD_mse <- mean((TLD_test-TLD_forecast$mean)^2)
TLD_mse

plot(TLD_forecast)


## S&P 500
df <- read.csv('/Users/shahsiangchuan/Downloads/HistoricalData_1683073126605.csv')
stock <- df[,c('Close.Last')]
stock <- rev(stock)
#stock$Date <-strptime(stock$Date,"%m/%d/%Y")


### Seasonality on quater.
stock_ts <- ts(stock,frequency = 66)
stock_ts <- (stock_ts - mean(stock_ts))/sd(stock_ts)
adf.test(stock_ts)
plot(stock_ts,type='l')

stock_decompose <- decompose(stock_ts)
plot(stock_decompose)


stock_adjusted <- stock_ts - stock_decompose$seasonal
plot(stock_adjusted)


stock_train <- stock_ts[-1:-20]
stock_test <- rev(stock_ts)[1:20]

## With seasonality
stock_arima <- auto.arima(stock_train, seasonal=T)
stock_forecast <- forecast(stock_train, length(stock_test))
stock_mse <- mean((stock_test-stock_forecast$mean)^2)
stock_mse


## Without seasonality
stock_arima <- auto.arima(stock_train, seasonal=F)
stock_forecast <- forecast(stock_train, length(stock_test))
stock_mse <- mean((stock_test-stock_forecast$mean)^2)
stock_mse
plot(stock_forecast)

## Google Trend


google <- read.csv('/Users/shahsiangchuan/Downloads/google_trend_ai.csv')
plot(google,type='l')

google_ts <- ts(google,frequency = 52)
google_ts


google_decomp <- decompose(google_ts)
plot(google_decomp)


google_train <- google_ts[-1:-4]
google_test <- rev(google_ts)[1:4]

google_adjusted = google_ts - google_decomp$seasonal
plot(google_adjusted)

## With Seasonality
google_arima <- auto.arima(google_train, seasonal=T)
google_forcast <- forecast(google_arima, length(google_test))
google_mse <- mean((google_forcast$mean - google_test)^2)
google_mse

plot(google_forcast)

## Without Seasonality
google_arima <- auto.arima(google_test, seasonal=F)
google_forcast <- forecast(google_arima, length(google_test))
google_mse <- mean((google_forcast$mean - google_test)^2)
google_mse


