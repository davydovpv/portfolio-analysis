library(R.cache)

# Pulls stock history from Quandl using the Yahoo dataset.
# History is sorted and parsed into a data.frame with Date and Closing price
get.stock.history.raw <- function(symbol, start, end) {
  allData <- Quandl(paste0("YAHOO/", symbol), trim_start=start, trim_end=end, sort_order="asc", authcode="DtnoHM-ZkzznSdTzty8L", collapse="weekly")
  filteredData <- data.frame(
    Date = allData[,'Date'],
    Close = allData[,'Adjusted Close'])  
  filteredData
}

# Expose cached functions 
get.stock.history <- addMemoization(get.stock.history.raw)


get.equity.market.raw <- function(start, end) {
  allData <- Quandl("YAHOO/INDEX_SPY", trim_start=start, trim_end=end, sort_order="asc", authcode="DtnoHM-ZkzznSdTzty8L", collapse="weekly")
  filteredData <- data.frame(
    Date = allData[,'Date'],
    Close = allData[,'Adjusted Close'])  
  filteredData
}

get.equity.market <- addMemoization(get.equity.market.raw)

get.risk.free.raw <- function(start, end) {
  allData <- Quandl("YAHOO/INDEX_TNX", trim_start="2004-01-01", trim_end="2013-12-31", sort_order="asc", authcode="DtnoHM-ZkzznSdTzty8L", collapse="weekly")
  filteredData <- data.frame(
    Date = allData[,'Date'],
    Close = allData[,'Adjusted Close'])  
  filteredData
}

get.risk.free <- addMemoization(get.risk.free.raw)