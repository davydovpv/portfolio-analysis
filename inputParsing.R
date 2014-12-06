# Returns a vector of selected symbols
# input$symbols is a comma separated string, potentially with spaces
getSelectedSymbols <- function(input) {
  symbols <- sapply(strsplit(input$symbols, ","), function(str) {gsub("(^ +)|( +$)", "", str)}, simplify=FALSE)[[1]]
  symbols
}

getStartDate <- function(input) {
  input$dates[1]
}

getEndDate <- function(input) {
  input$dates[2]
}