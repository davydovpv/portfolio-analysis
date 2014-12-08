shinyUI(fluidPage(
  headerPanel("Dynamic number of plots"),
  mainPanel(
    sliderInput("n", "Number of plots", value=1, min=1, max=5),
    # This is the dynamic UI for the plots
    uiOutput("maPlots")
  )
))