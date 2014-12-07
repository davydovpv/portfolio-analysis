library(shiny)

source('linechart.R')

shinyUI(fluidPage(
  
  titlePanel("Project 281H"),
  
  sidebarLayout(
    sidebarPanel(
      p("Financial Analysis over a Timer Period using the R"),
      textInput("symbols", label = h6("Symbol(s)"), value = "XOM, IBM, AAL"),
      dateRangeInput("dates", label = p("Date range: yyyy-mm-dd"), format="yyyy-mm-dd", start="2013-11-24", end="2014-11-24"),
      submitButton(text = "Submit")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
          tabPanel("Home", 
            plotOutput("oldTimeseriesPlot")), 
          
          tabPanel("Statistics", fluidPage(
            fluidRow(h4("Stock Market")),
            fluidRow(tableOutput("statisticsStockMarketTable")),
            fluidRow(h4("Stocks")),
            fluidRow(tableOutput("statisticsStocksTable")),
            fluidRow(h4("Portfolio")),
            fluidRow(tableOutput("statisticsPortfolioTable")),
            fluidRow(
              column(width=6, h6("Correlation")),
              column(width=6, h6("Covariance"))
            )),
            fluidRow(
              column(width=6, verbatimTextOutput("statisticsPortfolioCorrelation")),
              column(width=6, verbatimTextOutput("statisticsPortfolioCovariance"))
            )),
          
          tabPanel("Moving Averages", 
            p("Coming soon..."),
            plotOutput("maPlots")), 
          
          tabPanel("Forecasting",
            p("Coming soon...")),
          
          tabPanel("Optimization",
            plotOutput("optimizationFeasiblePlot"))
      )
    )
  )
))
