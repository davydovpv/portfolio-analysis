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
  rate <- estimate.annualized.rate.of.return(rk, m)
  rate
}

estimate.annualized.rate.of.return <- function(rates, m = 52) {
  a <- mean(rates)
  alpha <- m * a
  alpha
}

get.geometric.mean <- function(col) {
  n <- length(col)
  term <- col[length(col)]/col[1]
  term^(52/n) - 1
}

to.percent <- function(d) {
  paste0(round(d*100, 2), "%")
}

convert.prices.to.returns <- function(dataframe) {
  rates <- get.rates.of.return(dataframe$Close)
  dates <- (dataframe$Date)[-1]
  returns <- data.frame(
    Date = dates,
    Close = rates)
}

merge.histories <- function(histories, symbols, includeDates) {
  if (length(histories) < 2) return ((histories[1])[2])
  
  merged <- histories[1]
  last <- histories[1]
  
  for(i in 2:length(histories)) {
    merged <- merge(last, histories[i], by="Date", all=TRUE)
    colnames(merged) <- c("Date", symbols[1:i])
    last <- merged
  }
  
  if (includeDates) {
    return (merged)
  } else {
    return(merged[2:length(merged)])
  }
}

get.closing.prices <- function(symbols, start, end, includeDates=FALSE) {
  histories <- lapply(symbols, get.stock.history, start=start, end=end)  
  closing.prices <- merge.histories(histories, symbols, includeDates)  
  closing.prices
}

get.closing.returns <- function(symbols, start, end) {
  closing.prices <- get.closing.prices(symbols, start, end)
  closing.returns <- apply(closing.prices, 2, get.rates.of.return)
  data.frame(closing.returns)
}

get.weights.raw <- function(sum, num, delta) {
  
  if (num <= 1) {
    print("Error: num < 2")
    return (NULL)
  }
  
  if (num == 2) {
    return (lapply(seq(0, sum, delta), function(i) {
      c(i, sum-i)
    }))
  }
  
  if (num >= 3) {
    return(unlist((lapply(seq(0, sum, delta), function(i) {
      return (lapply(get.weights(sum-i, num-1, delta), function(rest) {
        return (c(i, rest))
      }))
    })), recursive = F)) 
  }
  
}

get.weights <- addMemoization(get.weights.raw)
