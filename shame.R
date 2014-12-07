# This should all go away.....
library(TTR)
library(forecast)
library(lattice)
library(quadprog)

# Given the data frame of all stock data, selects just the data
# for the given symbol.  Returns a stock.data data.frame
get.data.for.symbol <- function(all.data, stock.symbol) {
  return(all.data[,stock.symbol])
}

# Takes a data.frame of stock data and converts it to a timeseries
convert.to.ts <- function(stock.data) {
  closing.prices <- stock.data$Close
  adj.freq <- round(length(closing.prices) / 10)
  start <- c(2004, 1)
  data.ts <- ts(closing.prices, start=start, freq=adj.freq)
  return (data.ts)
}

# Takes a data.frame of stock data, adds simple moving averages over
# two windows, and returns a timeseries with all three vectors
convert.to.ts.with.sma <- function(stock.data) {
  closing.prices <- stock.data
  sma50 <- SMA(closing.prices, n=7) 
  sma200 <- SMA(closing.prices, n=29)
  
  df <- data.frame(closing.prices, sma50, sma200)
  
  adj.freq <- round(length(df$closing.prices) / 10)
  start <- c(2004, 1)
  data.ts <- ts(df, start=start, freq=adj.freq)
  
  return(data.ts)
}

# Takes stock data and the symbol name, and plots it as a timeseries
plot.timeseries <- function(stock.data, stock.symbol) {
  stock.ts <- convert.to.ts(stock.data)
  
  ts.plot(stock.ts, main=stock.symbol, xlab="Time", ylab="Closing Price", col=c('Black'))
  legend('bottomright', c('Close'), lwd=2, col=c('Black'))
}

# Takes stock data and the symbol name, and plots it with simple moving averages
plot.timeseries.with.sma <- function(stock.data, stock.symbol) {
  stock.ts <- convert.to.ts.with.sma(stock.data)
  
  title <- paste("50 and 200 Day Moving Averages:", stock.symbol)
  ts.plot(stock.ts, main=title, xlab="Time", ylab="Closing Price", col=c('Black', 'Blue', 'Green'))
  legend('bottomright', c('Close', '50 Day', '200 Day'), lwd=2, col=c('Black', 'Blue', 'Green'))
}

# Uses various techniques to analyze and fit models to the data, and prints out
# which of the models are a good fit for the data.
analyze.timeseries.models <- function(stock.data, stock.symbol) {
  timeseries <- convert.to.ts(stock.data)
  
  # Decompose the Timeseries
  print(paste("Decomposed Timeseries for", stock.symbol))
  decomposed <- decompose(timeseries)
  plot(decomposed)
  
  # Single Exponential
  print(paste("Fitting Single Exponential Model for", stock.symbol))
  single.forecasts <- HoltWinters(timeseries, beta=F, gamma=F)
  single.forecasts2 <- forecast.HoltWinters(single.forecasts, h=48)
  single.acf <- acf(single.forecasts2$residuals, lag.max=20)
  single.box.test <- Box.test(single.forecasts2$residuals, lag=20, type="Ljung-Box")
  single.p <- single.box.test$p.value
  
  plot(single.forecasts2$residuals)
  hist(single.forecasts2$residuals)
  print(paste("P-Value for Single Exponential Model:", single.p))
  
  # Holt
  print(paste("Fitting Holt's Exponential Model for", stock.symbol))
  holt.forecasts <- HoltWinters(timeseries, beta=T, gamma=F)
  holt.forecasts2 <- forecast.HoltWinters(holt.forecasts, h=48)
  holt.acf <- acf(holt.forecasts2$residuals, lag.max=20)
  holt.box.test <- Box.test(holt.forecasts2$residuals, lag=20, type="Ljung-Box")
  holt.p <- holt.box.test$p.value
  
  plot(holt.forecasts2$residuals)
  hist(holt.forecasts2$residuals)
  print(paste("P-Value for Holt's Exponential Model:", holt.p))
  
  # Holt Winters
  print(paste("Fitting Holt Winters Model for", stock.symbol))
  holt.winters.forecasts <- HoltWinters(timeseries, beta=T, gamma=F)
  holt.winters.forecasts2 <- forecast.HoltWinters(holt.winters.forecasts, h=48)
  holt.winters.acf <- acf(holt.winters.forecasts2$residuals, lag.max=20)
  holt.winters.box.test <- Box.test(holt.winters.forecasts2$residuals, lag=20, type="Ljung-Box")
  holt.winters.p <- holt.winters.box.test$p.value
  
  plot(holt.winters.forecasts2$residuals)
  hist(holt.winters.forecasts2$residuals)
  print(paste("P-Value for Holt Winters Model:", holt.winters.p))
  
  # Find Best Model
  threshold <- .1
  
  if (single.p > threshold) {
    print(paste0("Single Exponential Model is a good fit for ", stock.symbol))
    plot(single.forecasts2)
  } else {
    print(paste0("Single Exponential Model is not a good fit for ", stock.symbol))
  }
  
  if (holt.p > threshold) {
    print(paste0("Holt's Double Exponential Model is a good fit for ", stock.symbol))
    plot(holt.forecasts2)
  } else {
    print(paste0("Holt's Double Exponential Model is not a good fit for ", stock.symbol))
  }
  
  if (holt.winters.p > threshold) {
    print(paste0("Holt Winters's Exponential Model is a good fit for ", stock.symbol))
    plot(holt.winters.forecasts2)
  } else {
    print(paste0("Holt Winters's Exponential Model is not a good fit for ", stock.symbol))
  }
  
}

analyze.financial.data <- function(stock.data, stock.symbol) {
  closing.prices <- stock.data$Close
  
  alpha <- get.annualized.rate.of.return(closing.prices)
  sigma <- get.rates.st.dev(closing.prices)
  
  print(paste("Analzying Rates of Return for", stock.symbol))
  print(paste("  Annualized Rate of Return:", alpha))
  print(paste("  Standard Deviations:", sigma))
  
  plot.rates.of.return(closing.prices)
}

# Gets the rates of return for the price history of a stock
get.rates.of.return <- function(price.history) {
  rk <- diff(price.history)/price.history[-length(price.history)]
  return(rk)
}

# Approximates the annualized rate of return for the price history
# m: number of observations per period
get.annualized.rate.of.return <- function(price.history, m = 52) {
  rk <- get.rates.of.return(price.history)
  a <- mean(rk)
  alpha <- m * a
  return(alpha)
}

# Finds the standard deviation of the rates of return for the price history
# m: number of observations per period
get.rates.st.dev <- function(price.history, m = 52) {
  rk <- get.rates.of.return(price.history)
  st.dev <- sd(rk)
  sigma <- sqrt(m) * st.dev
  return(sigma)
}

# Plots a histogram of rates of return
plot.rates.of.return <- function(price.history) {
  rates.of.return <- get.rates.of.return(price.history)
  hist(rates.of.return)
}

analyze.stock <- function(all.data, stock.symbol) {
  print(paste("Beginning analysis for", stock.symbol, "------------"))
  
  # Get just the data for this stock symbol
  stock.data <- get.data.for.symbol(all.data, stock.symbol)
  
  # Convert to ts and plot
  plot.timeseries(stock.data, stock.symbol)
  
  # Plot TimeSeries with Simple Moving Averages
  plot.timeseries.with.sma(stock.data, stock.symbol)
  
  # Analyze Models and print which are best
  analyze.timeseries.models(stock.data, stock.symbol)
  
  # Analyze Rates of Return
  analyze.financial.data(stock.data, stock.symbol)
  
  print(paste("Ending analysis for", stock.symbol, "------------"))
}