library(shiny)
library(Quandl)

source('inputParsing.R')

source('dataAccess.R')
source('functions.R')
source('timeseries.R')

shinyServer(function(input, output, session) {
  
  # Reactive -----------------------
  
  currentPrices <- reactive({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    prices <- get.closing.prices(symbols, start, end)
    prices
  })
  
  currentPricesWithDates <- reactive({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    prices <- get.closing.prices(symbols, start, end, includeDates=TRUE)
    prices
  })
  
  currentReturns <- reactive({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    returns <- get.closing.returns(symbols, start, end)
    returns
  })
  
  # Debug ----------------
  
  output$debug <- renderTable({
    #currentPrices()
    msft <- Quandl(paste0("YAHOO/", "MSFT"), trim_start= getStartDate(input), trim_end= getEndDate(input), sort_order="asc", authcode="DtnoHM-ZkzznSdTzty8L", collapse="weekly")
    data.frame(
      Date = msft[,'Date'])
  })
  
  # Timeseries Page --------------- 
  
#   output$timeseriesPlot <- renderLineChart({
#     #currentPrices()
#     data.frame(
#       Sine = sin(1:100/10 + 1 * pi/180) * 1,
#       Cosine = 0.5 * cos(1:100/10),
#       "Sine 2" = sin(1:100/10) * 0.25 + 0.5
#     )
#   })
  
  output$timeseriesPlot <- renderLineChart({
    currentPricesWithDates()
  })
  
})