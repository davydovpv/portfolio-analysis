# Gets the rates of return for the price history of a stock
get.rates.of.return <- function(price.history) {
  rk <- diff(price.history)/price.history[-length(price.history)]
  rk
}

# Converts 10 year interest rates into daily interest rates
get.weekly.interest.rates <- function(rate.history) {
  weekly <- (rate.history/100 + 1)^(1/52) - 1
  weekly
}

# Approximates the annualized rate of return for the price history
# m: number of observations per period
get.annualized.rate.of.return <- function(price.history, m = 52) {
  rk <- get.rates.of.return(price.history)
  a <- mean(rk)
  alpha <- m * a
  return(alpha)
}

merge.histories <- function(histories, symbols) {
  if (length(histories) < 2) return ((histories[1])[2])
  
  merged <- histories[1]
  last <- histories[1]
  
  for(i in 2:length(histories)) {
    merged <- merge(last, histories[i], by="Date", all=TRUE)
    colnames(merged) <- c("Date", symbols[1:i])
    last <- merged
  }
  
  return(merged[2:length(merged)])
}

get.closing.prices <- function(symbols, start, end) {
  histories <- lapply(symbols, get.stock.history, start=start, end=end)  
  closing.prices <- merge.histories(histories, symbols)  
  closing.prices
}

get.closing.returns <- function(symbols, start, end) {
  closing.prices <- get.closing.prices(symbols, start, end)
  closing.returns <- apply(closing.prices, 2, get.rates.of.return)
  closing.returns
}