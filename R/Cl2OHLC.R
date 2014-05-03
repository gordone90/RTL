Cl2OHLC <- function (x1) 
{
  if(!is.xts(x1)) {stop("series is not xts format")}
  x1<-cbind(x1,x1,x1,x1)
  names(x1)<- c("Open","High","Low","Close")
  return(x1)
}