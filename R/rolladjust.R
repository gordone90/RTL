#' \code{rolladjust} 
#' @description Returns a xts price or return object adjusted for contract roll. The methodology used to adjust returns is to remove the daily returns on the day after expiry and for prices to adjust historical rolling front month contracts by the size of the roll at each expiry. This is conducive to quantitative trading strategies as it reflects the PL of a financial trader. 
#' @param x An xts object of prices or returns.
#' @param datatype Type of xts input object: "prices" or "returns".
#' @param commodityname Name of commodity in expiry_table. See example below for values.
#' @param rolltype Type of contract roll: "Last.Trade" or "First.Notice".
#' @param secondcontract Object name of to adjust roll price with if datatype = "prices".
#' @param option TRUE if you also want to remove option roll returns. Default is FALSE.
#' @return Roll-adjusted xts object of returns
#' @export rolladjust
#' @author Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>
#' @examples 
#' data(data)
#' unique(expiry_table$cmdty) # for list of commodity names
#' # To adjust returns
#' x <- RTL:::data_ret(x=CL1,returntype=c("absolute"))
#' RTL:::rolladjust(x=x,datatype=c("returns"),commodityname=c("cmewti"),rolltype=c("Last.Trade"),secondcontract=NULL,option=FALSE)
#' # To adjust prices
#' RTL:::rolladjust(x=Cl(CL1),datatype=c("prices"),commodityname=c("cmewti"),rolltype=c("Last.Trade"),secondcontract=Cl(CL2),option=FALSE)

rolladjust <- function (x,datatype=c("returns"),commodityname=c("cmewti"),rolltype=c("Last.Trade"),secondcontract=NULL,option=FALSE) {
  check <- unique(expiry_table$cmdty) ; if (!(commodityname %in% check)) stop("Unknown commodityname: Type unique(expiry_table$cmdty) for available selection")
  if (class(x)[1]!="xts") stop("Returns time series in NOT an xts class")
  if (!(rolltype %in% c("Last.Trade","First.Notice"))) stop("Incorrect rolltype specified")
  
  seriesname <- names(x)
  
  table<- subset(expiry_table,cmdty==commodityname)
  if (rolltype=="Last.Trade") {table <- table$Last.Trade}
  if (rolltype=="First.Notice") {table <- table$First.Notice}
  
  x$expiry <- 0
  for (i in 1:length(table)) {
    x$expiry[as.Date(index(x))==table[i]] <- 1
    if (option==TRUE) {
      a<-table[i]
      days = seq(a, a-10, by="-1 day")
      days <- isHoliday(as.timeDate(days),holidays=holidayNYSE(unique(year(days))),wday=1:5)
      days <- names(days)[!days]
      days <- as.Date(days[2])
      x$expiry[as.Date(index(x))==days] <- 1
    }
  }

  if (datatype=="returns") {
    x$expiry <- lag(x$expiry,lag=1,arithmetic=FALSE); x$expiry[is.na(x$expiry)]<- 0
    x <- x[x$expiry!=1] ; x$expiry <- NULL
  }
    
  if (datatype=="prices") {
    if (is.OHLC(x)) {
      x$dollarroll <- 0
      x<-na.omit(merge(x,Cl(secondcontract))) 
      for (i in 1:nrow(x)) {if(x$expiry[i,]==1){x$dollarroll[i,]<-x$Close[i,]- x$Close.1[i,] }}
      x$cumroll <- 0
      for (i in (nrow(x):1)) {x$cumroll[i,]<- last(cumsum(x$dollarroll[i:nrow(x)]))}
      #x$adj <- x$Close-x$cumroll
      x<-x[,1:4]-drop(x$cumroll)
    } else {
      names(x)<-c("data1","expiry")
      x$dollarroll <- 0
      x<-merge(x,secondcontract) ; names(x)[ncol(x)]<-c("data2")
      for (i in 1:nrow(x)) {if(x$expiry[i,]==1){x$dollarroll[i,]<-x$data1[i,]- x$data2[i,] }}
      x$cumroll <- 0
      for (i in (nrow(x):1)) {x$cumroll[i,]<- last(cumsum(x$dollarroll[i:nrow(x)]))}
      x$adj <- x$data1-x$cumroll
      x <- x$adj
    }
  }
  return(x)
}

