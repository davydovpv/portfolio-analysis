library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Project 281H"),
  
  sidebarLayout(
    sidebarPanel(
      p("Financial Analysis over a Timer Period using the R"),
      textInput("symbols", label = h6("Symbol(s)"), value = "XOM, IBM, AAL"),
      checkboxGroupInput("selectedAnalysis", label = h6("Analysis"), 
                         choices = list("Fundamental Analysis" = 1, "Time Series Analysis" = 2, "Portfolio Analysis" = 3),
                         selected = c(2)),
      dateRangeInput("dates", label = p("Date range: yyyy-mm-dd"), format="yyyy-mm-dd", start="2013-11-24", end="2014-11-24"),
      submitButton(text = "Submit")
    ),
    
    mainPanel(
      plotOutput("timeseriesPlot"),
      p("For this milestone of the application, we have implemented the simple time series analysis.  You can plot multiple stock symbols on the chart over a custom date range.  Currently, we only support weekly data.  Future iterations of this application will support more granular stock data configuration and additional analysis types."),
      textOutput("selectedPortfolios")
    )
  )
))
