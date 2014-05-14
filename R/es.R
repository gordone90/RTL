#' \code{es} 
#' @description Returns the Expected Shortfall risk based on a given confidence interval.
#' @param x Univariate xts series of returns.
#' @param ci Confidence interval i.e. .05 or .95
#' @return A numeric value. 
#' @export es
#' @author Philippe Cote <coteph@@mac.com>, Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' ret<-RTL:::data_ret(x=Cl(CL1),returntype=c("relative"))
#' RTL:::es(x=ret,ci=.95)
es <- function(x,ci) {
  x<-coredata(x) ; x<-na.omit(x) ; x<-sort(x)
  cnt<-length(x) ; cnt
  r<-round(ci*cnt)
  if (ci>.5) {es<-mean(x[r:cnt])} else {es<-mean(x[1:r])}
  return(es)
}