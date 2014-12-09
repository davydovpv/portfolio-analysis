library(TTR)

graph.colors <- c('Black', 'Blue', 'Green', 'Red', 'Purple', 'Brown')

# Takes a data.frame of stock data, adds simple moving averages over
# two windows, and returns a timeseries with all three vectors
convert.to.ts.with.sma <- function(closing.prices, start, fast=50, slow=200) {
  sma50 <- SMA(closing.prices, n=round(fast/7))
  sma200 <- SMA(closing.prices, n=round(slow/7)) 
  
  df <- data.frame(closing.prices, sma50, sma200)
  
  adj.freq <- 52
  
  parts <- unlist(strsplit(format(start, format="%Y-%m-%d"), "-"))
  year <- as.numeric(parts[1])
  month <- as.numeric(parts[2])
  days <- as.numeric(parts[3])     
  weeks <- round(((month - 1) * 30 + days) / 7)
  
  start.vector <- c(year, weeks)
  
  data.ts <- ts(df, start=start.vector, freq=adj.freq)  
  data.ts
}

# Takes stock data and the symbol name, and plots it with simple moving averages
plot.timeseries.with.sma <- function(closing.prices, stock.symbol, start, fast=50, slow=200) {
  stock.ts <- convert.to.ts.with.sma(closing.prices, start, fast, slow)
  
  title <- paste(paste(fast, "and", slow, "Day Moving Averages:"), stock.symbol)
  ts.plot(stock.ts, main=title, xlab="Time", ylab="Closing Price", col=graph.colors)
  legend('bottomright', c('Close', paste(fast, 'Day'), paste(slow, 'Day')), lwd=2, col=graph.colors)
}