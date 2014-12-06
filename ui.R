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
          tabPanel("Home", plotOutput("oldTimeseriesPlot")), 
          tabPanel("Statistics"),
          tabPanel("Moving Averages"), 
          tabPanel("Forecasting"),
          tabPanel("Optimization")
      )
    )
  )
))
