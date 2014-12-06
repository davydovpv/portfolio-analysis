library(shiny)
library(Quandl)

# Returns a vector of selected symbols
getSelectedSymbols <- function(input) {
  symbols <- sapply(strsplit(input$symbols, ","), function(str) {gsub("(^ +)|( +$)", "", str)}, simplify=FALSE)[[1]]
  #symbols <- strsplit(input$symbols, "\ |,")[[1]]
  symbols
}

getDates <- function(input) {
  input$dates
}

# Pulls stock history from Quandl using the Yahoo dataset.
# History is sorted and parsed into a data.frame with Date and Closing price
get.stock.history <- function(symbol, start, end) {
  allData <- Quandl(paste0("YAHOO/", symbol), trim_start=start, trim_end=end, sort_order="asc", authcode="DtnoHM-ZkzznSdTzty8L", collapse="weekly")
  filteredData <- data.frame(
    Date = allData[,'Date'],
    Close = allData[,'Adjusted Close'])  
  filteredData
}

shinyServer(function(input, output) {
  
  output$selectedPortfolios <- renderText({
    getSelectedSymbols(input)
  })
  
  output$timeseriesPlot <- renderPlot({
    symbols <- getSelectedSymbols(input)
    dates <- getDates(input)
    colors <- c('Black', 'Blue', 'Green', 'Red')
    
    histories <- lapply(symbols, get.stock.history, dates[1], dates[2])
    max.price <- max(sapply(histories, function(history) { max(history$Close) }))
    
    
    stock.history <- histories[[1]]
    plot(stock.history, type="l", main="Weekly Time Series", col=colors, ylim=c(0, max.price))
    
    if (length(symbols) > 1) {
      for (i in seq(2, length(symbols))) {
        stock.history <- histories[[i]]
        lines(stock.history, col=colors[i], type="l")
      }
    }
    
    legend('bottomright', symbols, lwd=2, col=colors)
  })
  
})
