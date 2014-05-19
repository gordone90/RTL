sdev <- function(x=data,ret="rel",roll=TRUE,cmdty="",iwindow) {
  # data is univariate
  # return type
  if (ret=="rel") { (x <- (x-lag(x,lag=1,arithmetic=FALSE))/lag(x,lag=1,arithmetic=FALSE))}
  if (ret=="abs") { (x <- x-lag(x,lag=1,arithmetic=FALSE))}
  
  # roll adjust
  if(roll==TRUE) {x <- rolladjust(x,datatype=c("returns"),commodityname=cmdty,rolltype=c("Last.Trade"))}
  
  # volatility computation
  tmp<-rollapply(x,width=iwindow,FUN=sd,by.column=TRUE)
  
  #fit <- garchFit(~garch(1,1),data=as.xts(na.omit(x)),trace=FALSE)
  # garchvol = as.matrix(volatility(fit,type="sigma")) 
  #garchvol <- fit@sigma.t  
  #x <- merge(na.omit(x),garchvol)
  if(ret=="rel") {
    # Annualized Volatilty
    if (periodicity(x)$scale=="daily") {out <- tmp*sqrt(250)}
    if (periodicity(x)$scale=="monthly") {out <- tmp*sqrt(12)}
  } else {out<-tmp}         
  return(out)
}