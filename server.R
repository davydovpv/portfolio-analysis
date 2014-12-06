library(shiny)
library(Quandl)

source('inputParsing.R')
source('dataAccess.R')

source('linechart.R')

shinyServer(function(input, output, session) {
  
  output$timeseriesPlot <- renderLineChart({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
  
    stockData <- get.closing.prices(symbols, start, end)
    
    return(stockData)    
  })
  
  output$oldTimeseriesPlot <- renderPlot({
    symbols <- getSelectedSymbols(input)
    colors <- c('Black', 'Blue', 'Green', 'Red')
    
    histories <- lapply(symbols, get.stock.history, getStartDate(input), getEndDate(input))
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