rolladjust <- function (tmp,datatype=c("prices"),commodityname=c("cmewti"),rolltype=c("Last.Trade"),secondcontract,option=FALSE) {
  
  # purpose: adjust returns for roll  
  # input: 
    # an xts object of prices or returns
    # data_type= "prices" or "returns"
    # commodity name = selection found by running "unique(expiry_table$cmdty)"
    # rolltype = c("Last.Trade","First.Notice")
    # second contract = contract to adjust roll price with if datatype = prices
    # option = TRUE if you also want to remove option roll returns
   # output: roll adjusted xts object of returns
  
  ### INPUT CHECKS
  check <- unique(expiry_table$cmdty) ; if (!(commodityname %in% check)) stop("Unknown commodityname: Type unique(expiry_table$cmdty) for available selection")
  if (class(tmp)[1]!="xts") stop("Returns time series in NOT an xts class")
  if (!(rolltype %in% c("Last.Trade","First.Notice"))) stop("Incorrect rolltype specified")
  
  seriesname <- names(tmp)

  
  table<- subset(expiry_table,cmdty==commodityname)
  if (rolltype=="Last.Trade") {table <- table$Last.Trade}
  if (rolltype=="First.Notice") {table <- table$First.Notice}
  
  tmp$expiry <- 0
  for (i in 1:length(table)) {
    tmp$expiry[as.Date(index(tmp))==table[i]] <- 1
    if (option==TRUE) {
      x<-table[i]
      days = seq(x, x-10, by="-1 day")
      days <- isHoliday(as.timeDate(days),holidays=holidayNYSE(unique(year(days))),wday=1:5)
      days <- names(days)[!days]
      days <- as.Date(days[2])
      tmp$expiry[as.Date(index(tmp))==days] <- 1
    }
  }

  if (datatype=="returns") {
    tmp$expiry <- lag(tmp$expiry,lag=1,arithmetic=FALSE); tmp$expiry[is.na(tmp$expiry)]<- 0
    tmp <- tmp[tmp$expiry!=1] ; tmp$expiry <- NULL
  }
    
  if (datatype=="prices") {
    if (is.OHLC(tmp)) {
      tmp$dollarroll <- 0
      tmp<-merge(tmp,Cl(secondcontract)) 
      for (i in 1:nrow(tmp)) {if(tmp$expiry[i,]==1){tmp$dollarroll[i,]<-tmp$Close[i,]- tmp$Close.1[i,] }}
      tmp$cumroll <- 0
      for (i in (nrow(tmp):1)) {tmp$cumroll[i,]<- last(cumsum(tmp$dollarroll[i:nrow(tmp)]))}
      #tmp$adj <- tmp$Close-tmp$cumroll
      tmp<-tmp[,1:4]-drop(tmp$cumroll)
    } else {
      names(tmp)<-c("data1","expiry")
      tmp$dollarroll <- 0
      tmp<-merge(tmp,secondcontract) ; names(tmp)[ncol(tmp)]<-c("data2")
      for (i in 1:nrow(tmp)) {if(tmp$expiry[i,]==1){tmp$dollarroll[i,]<-tmp$data1[i,]- tmp$data2[i,] }}
      tmp$cumroll <- 0
      for (i in (nrow(tmp):1)) {tmp$cumroll[i,]<- last(cumsum(tmp$dollarroll[i:nrow(tmp)]))}
      tmp$adj <- tmp$data1-tmp$cumroll
      tmp <- tmp$adj
    }
  }
    
  return(tmp)
}

