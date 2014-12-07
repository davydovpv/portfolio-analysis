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

