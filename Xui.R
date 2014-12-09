library(dplyr)
library(ggvis)

shinyUI(fluidPage(
  headerPanel("Feasible Portfolios"),
  mainPanel(
    dateRangeInput("dates", label = h3("Date range"), start="2013-01-01", end="2014-01-01"),
    textInput("selectedSymbols", "Symbols", "MSFT,AAPL,IBM"),
    ggvisOutput("optimizationPlot")
  )
))