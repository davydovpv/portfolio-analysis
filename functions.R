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
  closing.returns
}

get.erp.raw <- function(start, end) {
  equity.market <- get.equity.market(start, end)
  risk.free <- get.risk.free(start, end)
  
  equity.market.rates <- get.rates.of.return(equity.market$Close)
  equity.market.dates <- (equity.market$Date)[-1]
  equity.market.returns <- data.frame(
    Date = equity.market.dates,
    Market = equity.market.rates)
  
  risk.free.rates <- get.weekly.interest.rates(risk.free$Close)
  risk.free.dates <- (risk.free$Date)
  risk.free.returns <- data.frame(
    Date = risk.free.dates,
    Rf = risk.free.rates)
  
  merged <- merge(equity.market.returns, risk.free.returns, by.x = 'Date', by.y = 'Date')
  premiums <- merged[,'Market'] - merged[,'Rf']  
  
  erp <- 52 * mean(premiums) # Approximate an annualized return
  erp
}

get.erp <- addMemoization(get.erp.raw)

get.beta.raw <- function(symbol, start, end) {
  stock.history <- get.stock.history(symbol, start, end)
  equity.market <- get.equity.market(start, end)
  
  stock.history.rates <- get.rates.of.return(stock.history$Close)
  stock.history.dates <- (stock.history$Date)[-1]
  stock.history.returns <- data.frame(
    Date = stock.history.dates,
    Close = stock.history.rates)
  
  equity.market.rates <- get.rates.of.return(equity.market$Close)
  equity.market.dates <- (equity.market$Date)[-1]
  equity.market.returns <- data.frame(
    Date = equity.market.dates,
    Market = equity.market.rates)
  
  merged <- merge(stock.history.returns, equity.market.returns, by.x = 'Date', by.y = 'Date')
  
  regression <- lm(merged[,'Close'] ~ merged[,'Market'])
  b <- regression$coefficients[[2]]
  
  b
}

get.beta <- addMemoization(get.beta.raw)

get.annualized.Rf <- function() {
  risk.free <- get.risk.free(start, end)
  risk.free.rates <- get.weekly.interest.rates(risk.free$Close)
  rf <- estimate.annualized.rate.of.return(risk.free.rates)
  rf
}

get.expected.return <- function(symbol, start, end) {
  erp <- get.erp(start, end)
  beta <- get.beta(symbol, start, end)
  rf <- get.annualized.Rf()
  
  annualized.Re <- rf + beta * erp
  annualized.Re
}