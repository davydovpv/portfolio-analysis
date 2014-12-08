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


# -------

get.equity.market.return <- function() {
  market <- get.equity.market(start, end)
  closing <- market$Close
  get.annualized.rate.of.return(closing)
}

get.industries <- function(symbols) {
  symbols
}

get.betas <- function (symbols, start, end) {
  symbols
}

get.annual.returns <- function (symbols, start, end) {
  symbols
}

get.expected.returns <- function(symbols, start, end) {
  symbols
}

get.profit <- function(closing.prices, start, end) {
  colnames(closing.prices)
}

get.sharpe.ratios <- function(symbols, start, end) {
  symbols
}