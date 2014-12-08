library(shiny)
library(Quandl)

source('inputParsing.R')
source('linechart.R')

source('dataAccess.R')
source('functions.R')

shinyServer(function(input, output, session) {
  
  output$debug <- renderText({
    getSelectedWeights(input)
  })
  
  currentPrices <- reactive({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    prices <- get.closing.prices(symbols, start, end)
    prices
  })
  
  currentReturns <- reactive({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    returns <- get.closing.returns(symbols, start, end)
    returns
  })
  
  # Timeseries Page --------------- 
  
  
  
  
})