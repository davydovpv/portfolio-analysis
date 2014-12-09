library(ggvis)
library(dplyr)

shinyUI(fluidPage(
  headerPanel("THings"),
  mainPanel(
    dateRangeInput("dates", label = h3("Date range"), start="2013-01-01"),
    textInput("selectedSymbols", "Symbols", "MSFT,AAPL,IBM"),
    ggvisOutput("optimizationPlot")
  )
))