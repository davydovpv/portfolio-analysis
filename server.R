library(shiny)
library(Quandl)

source('inputParsing.R')
source('linechart.R')

source('dataAccess.R')
source('functions.R')

shinyServer(function(input, output, session) {
  
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
  
  
  
  # Statistics Page ---------------
  
  output$statisticsStockMarketTable <- renderTable({
    marketTable <- data.frame(
      erp = c("6.34%"),
      ret = c("8.3%"),
      sig = c("3.4%"))
    
    colnames(marketTable) <- c("ERP", "Return", "Standard Deviation")
    
    marketTable
  })
  
  output$statisticsStocksTable <- renderTable({
    stocksTable <- data.frame(
      Symbol = c('MSFT', 'AAPL'),
      Industry = c('Software', 'Software'),
      Beta = c(0,0),
      AnnualReturn = c(0,0),
      ExpectedReturn = c(0,0),
      StandardDeviation = c(0,0),
      High = c(0,0),
      Low = c(0,0),
      Average = c(0,0),
      Profit = c(0,0),
      SharpeRatio = c(0,0)
      )
    
    stocksTable    
  })
  
  output$statisticsPortfolioTable <- renderTable({
    portfolioTable <- data.frame(
      AnnualReturn = c(0),
      ExpectedReturn = c(0),
      StandardDeviation = c(0)
    )
    
    portfolioTable    
  })
  
  output$statisticsPortfolioCorrelation <- renderText({
    cor(currentReturns())
  })
  
  output$statisticsPortfolioCovariance <- renderText({
    cov(currentReturns())
  })
  
  
  # Moving Averages ------------------
  
  output$maPlots <- renderPlot({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    stockData <- get.closing.prices(symbols, start, end)
    
    par(mfrow=c(length(symbols),1))
    
    for (i in 1:length(symbols)) {
      plot(cars, type="l", main="SMA")
    }
  })
  
  # Forecasting ----------------------
  
  # Optimization ---------------------
  
  output$optimizationFeasiblePlot <- renderPlot({
    plot(cars)
  })
  
  # -------------------------------
})