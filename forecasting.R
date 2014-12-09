library(forecast)

plot.linear.model <- function(dataframe, symbol) {
  model <- lm(dataframe[,2]~dataframe[,1])
  r2 <- summary(model)$r.squared 
  
  title1 <- paste0(symbol, ":  R^2=", round(r2, 2))
  title2 <- paste0("Price = ", round(summary(model)$coefficients[2], 3), " * Date + ", round(summary(model)$coefficients[1],1))
  
  plot(dataframe, type="l", main=paste0(title1, "\n", title2))
  abline(model, col="Blue")
}

get.date.vector <- function(d) {
  parts <- unlist(strsplit(format(d, format="%Y-%m-%d"), "-"))
  year <- as.numeric(parts[1])
  month <- as.numeric(parts[2])
  days <- as.numeric(parts[3])     
  weeks <- round(((month - 1) * 30 + days) / 7)
  
  d.vector <- c(year, weeks)
  d.vector
}

convert.to.ts <- function(dataframe) {
  start <- get.date.vector(dataframe$Date[1])
  
  closing.prices <- dataframe[,2]
  freq <- 52
  
  data.ts <- ts(closing.prices, start=start, freq=freq)
  data.ts  
}

plot.single.exp.model <- function(dataframe, symbol) {
  timeseries <- convert.to.ts(dataframe)
  
  single.forecasts <- HoltWinters(timeseries, beta=F, gamma=F)
  single.forecasts2 <- forecast.HoltWinters(single.forecasts, h=10)
  single.acf <- acf(single.forecasts2$residuals, lag.max=20)
  single.box.test <- Box.test(single.forecasts2$residuals, lag=20, type="Ljung-Box")
  single.p <- single.box.test$p.value
  
  title <- paste0(symbol, "\np-value=", round(single.p, 3))
  plot(single.forecasts2, main=title)  
}

plot.holt.model <- function(dataframe, symbol) {
  timeseries <- convert.to.ts(dataframe)
  
  holt.forecasts <- HoltWinters(timeseries, beta=T, gamma=F)
  holt.forecasts2 <- forecast.HoltWinters(holt.forecasts, h=48)
  holt.acf <- acf(holt.forecasts2$residuals, lag.max=20)
  holt.box.test <- Box.test(holt.forecasts2$residuals, lag=20, type="Ljung-Box")
  holt.p <- holt.box.test$p.value
  
  title <- paste0(symbol, "\np-value=", round(holt.p, 3))
  plot(holt.forecasts2, main=title)  
}

plot.holt.winters.model <- function(dataframe, symbol) {
  timeseries <- convert.to.ts(dataframe)
  
  holt.winters.forecasts <- HoltWinters(timeseries, beta=T, gamma=F)
  holt.winters.forecasts2 <- forecast.HoltWinters(holt.winters.forecasts, h=48)
  holt.winters.acf <- acf(holt.winters.forecasts2$residuals, lag.max=20)
  holt.winters.box.test <- Box.test(holt.winters.forecasts2$residuals, lag=20, type="Ljung-Box")
  holt.winters.p <- holt.winters.box.test$p.value
  
  title <- paste0(symbol, "\np-value=", round(holt.winters.p, 3))
  plot(holt.winters.forecasts2, main=title)  
}