# Returns a vector of selected symbols
# input$symbols is a comma separated string, potentially with spaces
getSelectedSymbols <- function(input) {
  symbols <- unlist(strsplit(input$selectedSymbols, ","))  
  symbols
}

getSelectedWeights <- function(input) {
  weightstrs <- strsplit(input$selectedWeights, ",")
  weights <- c(do.call("cbind",lapply(weightstrs, as.numeric))) 
  weights
}

getStartDate <- function(input) {
  input$dates[1]
}

getEndDate <- function(input) {
  input$dates[2]
}