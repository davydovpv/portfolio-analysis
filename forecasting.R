plot.linear.model <- function(dataframe, symbol) {
  plot(dataframe, type="l")
  res <- lm(dataframe[,2]~dataframe[,1])
  abline(res)
}