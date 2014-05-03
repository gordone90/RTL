varshockedvalues <- function(prices,relshocks) {

  # input: xts series of prices and relative shocks
  # output: shocked components. relative shock at time t multiplied by current prices
  
  p<-coredata(last(na.omit(last(prices,n=2))))
  r<-tail(relshocks)
  for (i in 1:ncol(p)) {relshocks[,i] <- as.numeric(p[,i]) * relshocks[,i]}
  return(relshocks)
  
}