clean_series <- function(x,cmdty="cmewti",cents2dollars=FALSE,removezerovolumedays=FALSE,removezeroOPENdays=FALSE,roll=FALSE,closeonly=FALSE) {
  if(roll==TRUE) {
    a<-cmdty ; x2=paste(unique(gsub("[0-9]","",as.character(subset(tickers,cmdty == a)$R_ticker))),"2",sep="")
    a<-rolladjust(Cl(x),datatype=c("prices"),commodityname=cmdty,rolltype=c("Last.Trade"),secondcontract=Cl(get(x2)))
    rolladjustment<- Cl(x)-a
    x$Close <- x$Close-rolladjustment 
    x$High <- x$High-rolladjustment
    x$Low <- x$Low-rolladjustment
    x$Open <- x$Open-rolladjustment
  }
  if(!is.xts(x)) {stop("series is not xts format")}
  if(cmdty=="ags") {
    if(removezerovolumedays==TRUE) {x <- x[!Vo(x)==0]}# remove zero volume days
    x$Open <- ifelse(Op(x)==0,Cl(x)-Cl2Cl(Cl(x),type="absolute",k=0),Op(x))
    x$High <- ifelse(Hi(x)==0,max(Op(x),Cl(x)),Hi(x))
    x$Low <- ifelse(Lo(x)==0,min(Op(x),Cl(x)),Lo(x))
  }
 
  if(removezeroOPENdays==TRUE) {
    x <- x[!Op(x)==0] ; 
    if(cmdty=="cmewti") {x <- x[!Op(x)<10]}
    }# remove zero volume days
  
  if(cmdty=="fx") { # cleans up quandl FX. creates open=previous close and moves fwd high and low when missing
    names(x) <- c("Close","High","Low") ; x$Open <-0 ; x<-x[,c("Open","High","Low","Close")]
    x$Open <- ifelse(Op(x)==0,Cl(x)-Cl2Cl(Cl(x),type="absolute",k=0),Op(x))
    x$High[x$High==0,] <- NA ; x$Low[x$Low==0,] <- NA
    x<-na.locf(x,fromLast=F)
    }
  
  if(closeonly==TRUE) {
    x<-Cl2OHLC(x)
  }
  if(cents2dollars==TRUE){ 
    x$Open<- x$Open/100; x$High<- x$High/100 ;x$Low<- x$Low/100 ;x$Close<- x$Close/100 
  }
  return(x)
}