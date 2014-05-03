stlconvert <- function(x) {
  freq <- switch(periodicity(x)$scale,
                 daily=365,
                 weekly=52,
                 monthly=12,
                 quarterly=4,
                 yearly=1)
  
  pltStart <- as.POSIXlt(start(x))
  Start <- c(pltStart$year+1900,pltStart$mon+1)
  x1 <- ts(x[,1], start=Start, frequency=freq)
  return(x1)
}