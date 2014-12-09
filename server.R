library(shiny)
library(Quandl)
library(plyr)
library(ggvis)

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
    
    equity.market <- get.equity.market(start, end)
    
    market <- data.frame(
      erp = to.percent(get.geometric.mean(equity.market$Close) - get.annualized.Rf()),
      rf = to.percent(get.annualized.Rf()),
      ret = to.percent(get.geometric.mean(equity.market$Close)),
      sig = to.percent(sd(convert.prices.to.returns(equity.market)$Close))
      )
    
    colnames(market) <- c("ERP", "Risk Free Rate", "Annual Return", "Standard Deviation")
    market
  })
  
  output$statisticsPortfolio <- renderTable({
    symbols <- getSelectedSymbols(input)
    weights <- getSelectedWeights(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    comparison.table <- currentReturns()
    
    r <- matrix(comparison.table, nrow = length(symbols), ncol = 1)
    
    # Covariance Matrix
    C <- matrix(cov(comparison.table), nrow = length(symbols), ncol = length(symbols))
    w <- as.matrix(weights)
    
    p.sd <- sqrt(t(w) %*% C %*% w)
    
    data.frame(
      ExpectedReturn = to.percent(get.portfolio.mean(symbols, weights, start, end)),
      StandardDeviation = to.percent(p.sd))
  })
  
  output$statisticsStocks <- renderTable({
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    stock.prices <- currentPrices()
    stock.returns <- currentReturns()
    
    expected.returns <- get.expected.returns(symbols, start, end)
    standard.deviations <- as.vector(sapply(stock.returns, sd))
    rf <- get.annualized.Rf()
        
    stocksTable <- data.frame(
      Symbol = symbols,
      #Industry = get.industries(symbols),
      Beta = get.betas(symbols, start, end),
      AnnualReturn = sapply(get.annual.returns(stock.prices, start, end), function(g) { to.percent(g) }),
      ExpectedReturn = sapply(expected.returns, function(g) { to.percent(g) }),
      StandardDeviation = sapply(standard.deviations, function(g) { to.percent(g)}),
      Average = as.vector(sapply(stock.prices, mean)),
      High = as.vector(sapply(stock.prices, max)),
      Low = as.vector(sapply(stock.prices, min)),
      ProfitPerShare = get.profit(stock.prices, start, end),
      SharpeRatio = get.sharpe.ratios(expected.returns, standard.deviations, rf)
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
  
  # TEmp ---------------
  
  feasible.returns <- reactive({
    comparison.table <- currentReturns()
    symbols <- getSelectedSymbols(input)
    start <- getStartDate(input)
    end <- getEndDate(input)
    
    num <- length(symbols)
    
    r <- matrix(colMeans(comparison.table)*52, nrow = num, ncol = 1)
    C <- matrix(cov(comparison.table), nrow = num, ncol = num)
        
    expected.returns <- as.vector(sapply(symbols, get.expected.return, start=start, end=end))
        
    means <- c()
    sds <- c()  
    weights <- c()
    
    all.weights <- get.weights(1, num, .02)
    
    for (i in 1:length(all.weights)) {
      w <- matrix(all.weights[[i]], nrow=num, ncol=1)
      p.mean <- t(w) %*% r
      p.mean <- sum(expected.returns * as.vector(w))
      p.sd <- sqrt(t(w) %*% C %*% w)
      
      means <- c(means, p.mean)
      sds <- c(sds, p.sd)
      
      weightStrings <- c()
      for (j in 1:num) {
        ws <- paste0(symbols[j], ": ", round(w[j]*100,2), "%")
        weightStrings <- c(weightStrings, ws)
      }
      weight <- paste(weightStrings, collapse=" ")
      weights <- c(weights, weight)
    }
    
    feasible.returns <- data.frame(
      Sd = sds * 100,
      Mean = means * 100,
      Weight = weights)
    feasible.returns
  })
  
  tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$Mean)) return(NULL)
    
    all_data <- isolate(feasible.returns())
    tolerance <- .000001
    datum <- subset(all_data, abs(Mean - x$Mean) < tolerance & abs(Sd - x$Sd) < tolerance)
    if (is.null(datum)) return(NULL)
    
    paste0("<strong>", datum$Weight, "</strong><br/>",
           "Mean: ", round(datum$Mean, 2), "%<br/>",
           "St. Dev: ", round(datum$Sd, 2), "%")
  }
  
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    
    feasible.returns %>% 
      ggvis(x=~Sd, y=~Mean) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      add_tooltip(tooltip, "hover") %>%
      add_axis("x", title = "Standard Deviation (%)") %>%
      add_axis("y", title = "Mean Expected Return (%)") 
  })
  
  vis %>% bind_shiny("optimizationPlot")
  
  
})