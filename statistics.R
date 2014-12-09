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

get.expected.return.raw <- function(symbol, start, end) {
  erp <- get.erp(start, end)
  beta <- get.beta(symbol, start, end)
  rf <- get.annualized.Rf()
  
  annualized.Re <- rf + beta * erp
  annualized.Re
}

get.expected.return <- addMemoization(get.expected.return.raw)

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
  as.vector(sapply(symbols, get.beta, start=start, end=end))
}

get.annual.returns <- function (stock.prices, start, end) {
  geometric.means <- apply(stock.prices, 2, get.geometric.mean)
  as.vector(geometric.means)
}

get.expected.returns <- function(symbols, start, end) {
  rf <- get.annualized.Rf()
  erp <- get.erp(start, end)
  
  expected <- sapply(symbols, function(symbol) {
    beta <- get.beta(symbol, start, end)
    re <- rf + beta * erp
  })
  
  as.vector(expected)
}

get.profit <- function(closing.prices, start, end) {
  profits <- apply(closing.prices, 2, function(col) {
    col[length(col)] - col[1]
  })
  as.vector(profits)
}

get.sharpe.ratios <- function(expected.returns, standard.deviations, rf) {
  sharpes <- c()
  for(i in 1:length(expected.returns)) {
    s <- (expected.returns[i] - rf)/ standard.deviations[i]
    sharpes <- c(sharpes, s)
  }
  sharpes
}


# ------------

get.portfolio.mean <- function(symbols, weights, start, end) {
  sum <- 0
  for (i in 1:length(symbols)) {
    s <- symbols[i]
    w <- weights[i]
    
    re <- get.expected.return(s, start, end)
    sum <- sum + w * re
  }
  sum
}