aveOFave <- function (x, n = c(3,5,10), ...) 
{
  numberofave <- length(n)
  x$aveOFave <- 0
  for (i in 1:numberofave) {
    x$aveOFave <- x$aveOFave + (runMean(x[,1], n[i])*1/numberofave)
    }
    x
  return(x$aveOFave)
}