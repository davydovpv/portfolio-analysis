library(shiny)
library(Quandl)

source('inputParsing.R')

source('dataAccess.R')
source('functions.R')
source('timeseries.R')
source('statistics.R')
source('forecasting.R')

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
  
    
  # Timeseries Page --------------- 

  output$timeseriesPlot <- renderLineChart({
    currentPricesWithDates()
  })
  
  # Statistics Page ---------------
  
  output$statisticsMarket <- renderTable({
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    market <- data.frame(
      erp = get.erp(start, end),
      rf = get.annualized.Rf(),
      ret = c(0),
      sig = c(0)
      )
    
    colnames(market) <- c("ERP", "Risk Free Rate", "Return", "Standard Deviation")
    market
  })
  
  output$statisticsPortfolio <- renderTable({
    data.frame(
      Return = c(0),
      StDev = c(0))
  })
  
  output$statisticsStocks <- renderTable({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    stock.prices <- currentPrices()
    stock.returns <- currentReturns()
    
    stocksTable <- data.frame(
      Symbol = symbols,
      Industry = get.industries(symbols),
      Beta = get.betas(symbols, start, end),
      AnnualReturn = get.annual.returns(symbols, start, end),
      ExpectedReturn = get.expected.returns(symbols, start, end),
      StandardDeviation = as.vector(sapply(stock.prices, sd)),
      Average = as.vector(sapply(stock.prices, mean)),
      High = as.vector(sapply(stock.prices, max)),
      Low = as.vector(sapply(stock.prices, min)),
      Profit = get.profit(stock.prices, start, end),
      SharpeRatio = get.sharpe.ratios(symbols, start, end)
    )
    
    stocksTable
  })
  
  output$statisticsCorrelation <- renderTable({
    cor(currentReturns())
  })
  
  output$statisticsCovariance <- renderTable({
    cov(currentReturns()) * 100
  })
  
  # Moving Averages Page -----------
  
  output$maPlots <- renderUI({
    
    fast <- input$maFast
    slow <- input$maSlow
    
    symbols <- getSelectedSymbols(input)
    stock.prices <- currentPrices()
    start <- getStartDate(input)
        
    for (i in 1:length(symbols)) {
      local({
        index <- i
        plotname <- paste0("maPlot", index)
        
        output[[plotname]] <- renderPlot({
          plot.timeseries.with.sma(stock.prices[,index], symbols[index], start, fast, slow)
        })
        
      })
    }
    
    plot_output_list <- lapply(1:length(symbols), function(i) {
      plotname <- paste0("maPlot", i)
      plotOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items    
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Forecasting Page ---------------
  
  output$forecastLinearPlots <- renderUI({
    symbols <- getSelectedSymbols(input)
    stock.prices <- currentPricesWithDates()
    
    for (i in 1:length(symbols)) {
      local({
        index <- i
        plotname <- paste0("linearPlot", index)
        
        output[[plotname]] <- renderPlot({
          plot.linear.model(stock.prices[c(1, index+1)], symbols[index])
        })
        
      })
    }
    
    plot_output_list <- lapply(1:length(symbols), function(i) {
      plotname <- paste0("linearPlot", i)
      plotOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items    
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$forecastSingleExpPlots <- renderUI({
    symbols <- getSelectedSymbols(input)
    stock.prices <- currentPricesWithDates()
    
    for (i in 1:length(symbols)) {
      local({
        index <- i
        plotname <- paste0("singleExpPlot", index)
        
        output[[plotname]] <- renderPlot({
          plot.single.exp.model(stock.prices[c(1, index+1)], symbols[index])
        })
        
      })
    }
    
    plot_output_list <- lapply(1:length(symbols), function(i) {
      plotname <- paste0("singleExpPlot", i)
      plotOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items    
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$forecastHoltPlots <- renderUI({
    symbols <- getSelectedSymbols(input)
    stock.prices <- currentPricesWithDates()
    
    for (i in 1:length(symbols)) {
      local({
        index <- i
        plotname <- paste0("holtPlot", index)
        
        output[[plotname]] <- renderPlot({
          plot.holt.model(stock.prices[c(1, index+1)], symbols[index])
        })
        
      })
    }
    
    plot_output_list <- lapply(1:length(symbols), function(i) {
      plotname <- paste0("holtPlot", i)
      plotOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items    
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$forecastHoltWintersPlots <- renderUI({
    symbols <- getSelectedSymbols(input)
    stock.prices <- currentPricesWithDates()
    
    for (i in 1:length(symbols)) {
      local({
        index <- i
        plotname <- paste0("holtWintersPlot", index)
        
        output[[plotname]] <- renderPlot({
          plot.holt.winters.model(stock.prices[c(1, index+1)], symbols[index])
        })
        
      })
    }
    
    plot_output_list <- lapply(1:length(symbols), function(i) {
      plotname <- paste0("holtWintersPlot", i)
      plotOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items    
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Debug -----------
  
  output$debugReturns <- renderTable({
    
    currentPrices()
  })
  
  
})