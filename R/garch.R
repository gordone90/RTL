garch <- function(x=data,ret="rel",roll=TRUE,cmdty="") {
  # data is univariate
  # return type
  if (ret=="rel") { (x <- log(x/lag(x,lag=1,arithmetic=FALSE)))}
  if (ret=="abs") { (x <- x-lag(x,lag=1,arithmetic=FALSE))}
  if (ret=="flatprice") {x <- x}
  
  # roll adjust
  if(roll==TRUE) {x <- rolladjust(tmp=x,datatype=c("returns"),commodityname=cmdty,rolltype=c("Last.Trade"))}
    
  # volatility computation
  fit <- garchFit(~garch(1,1),data=as.xts(na.omit(x)),trace=FALSE)
  # garchvol = as.matrix(volatility(fit,type="sigma")) 
  garchvol <- fit@sigma.t
  
  x <- merge(na.omit(x),garchvol)
  if(ret=="rel") {
  # Annualized Volatilty
  if (periodicity(x)$scale=="daily") {x$garchvol <- x$garchvol*sqrt(250)}
  if (periodicity(x)$scale=="monthly") {x$garchvol <- x$garchvol*sqrt(12)}
  } else {
  if (periodicity(x)$scale=="daily") {x$garchvol <- x$garchvol}
  if (periodicity(x)$scale=="monthly") {x$garchvol <- x$garchvol}          
  }
  return(x$garchvol)
}


