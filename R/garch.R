#' \code{garch} 
#' @description Computes annualised Garch(1,1) volatilities using fGarch.
#' @param x Univariate or multivariate xts price series.
#' @param ret "rel" for relative returns, "abs" for absolute returns or "flatprice" if no transformation of x is require.
#' @return xts series of annualised Garch(1,1) volatilities if using relative returns.
#' @export garch
#' @author Philippe Cote <coteph@@mac.com>, Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::garch(x=Cl(CL1),ret="rel",roll=TRUE,cmdty="cmewti")
#' RTL:::garch(x=merge(Cl(CL1),Cl(CL24)),ret="rel",roll=TRUE,cmdty="cmewti")

garch <- function(x=data,ret="rel",roll=TRUE,cmdty="") {
  
  varnames <- colnames(x)
  
  if (ret=="rel") { (x <- log(x/lag(x,lag=1,arithmetic=FALSE)))}
  if (ret=="abs") { (x <- x-lag(x,lag=1,arithmetic=FALSE))}
  if (ret=="flatprice") {x <- x}
  
  if(roll==TRUE) {x <- rolladjust(x=x,datatype=c("returns"),commodityname=cmdty,rolltype=c("Last.Trade"))}
  x<-na.omit(x)
  nvar <- length(colnames(x))
  res <- x[,1] ; res[,1]<-0
  
  for (i in 1:nvar) {
    tmp<-x[,i]
    fit <- garchFit( ~ garch(1,1),data=as.xts(na.omit(tmp)),trace=FALSE)
    # garchvol = as.matrix(volatility(fit,type="sigma")) 
    garchvol <- fit@sigma.t
    res<- merge(na.omit(res),garchvol)
  }

  x<-res[,-1]
    
  if(ret=="rel") {
    if (periodicity(x)$scale=="daily") {x <- x*sqrt(250)}
    if (periodicity(x)$scale=="weekly") {x <- x*sqrt(52)}
    if (periodicity(x)$scale=="monthly") {x <- x*sqrt(12)}
  } 
  colnames(x) <- varnames
  return(x)
}


