#' \code{loadEIA} 
#' @description Returns a xts object representing the average of multiple averages.
#' @param tickers Dataframe in the same format as the tickers data object in data(data). 
#' @param api_key EIA token.
#' @param start_date Start date as character yyy-mm-dd.
#' @param end_date Start date as character yyy-mm-dd.
#' @return A univariate xts object for each tickers. 
#' @export loadEIA
#' @author Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::loadEIA(tickers=subset(tickers,source %in% "eia")[10,],api_key="83307AB94D4FC78A247EF9E5FAE67581",start_date="2003-01-01", end_date=format(Sys.Date(),"%Y-%m-%d"))

loadEIA <- function(tickers=tickers,api_key="83307AB94D4FC78A247EF9E5FAE67581",start_date="2003-01-01", end_date=format(Sys.Date(),"%Y-%m-%d")) {
  ticks_eia <- subset(tickers,source %in% "eia")
  for (i in 1:nrow(ticks_eia)) {
    tmp<-EIAscrape(as.character(ticks_eia[i,1]),api_key=api_key)
    assign(as.character(ticks_eia[i,2]),data.frame(tmp["data"]),envir = as.environment(1))
    print(as.character(ticks_eia[i,2]))     
  }   
  
  ### clean up periodicity
  L=as.list(as.matrix(subset(tickers,source %in% "eia")$R_ticker))
  for (l in L) {  
    tmp <- get(as.character(l))
    x <- nchar(tmp$data.Date[1])
    if (x==6) {
      for (i in 1:length(tmp$data.Date)) {tmp$data.Date[i] <- paste(tmp$data.Date[i],"01",sep="")}
      tmp$data.Date <- as.Date(tmp$data.Date, format="%Y%m%d")
      tmp<-xts(x=tmp[,2],order.by=tmp$data.Date) ; colnames(tmp)<- c(l)
      print(paste(l,strftime(index(tmp),format="%H:%M:%S",tz="America/New_York")[1]))
      assign(l,tmp) 
    }
    
    if (x==8) {
      tmp$data.Date <- as.Date(as.character(tmp$data.Date), format="%Y%m%d")
      tmp<-xts(x=tmp[,2],order.by=tmp$data.Date) ; colnames(tmp)<- c(l)
      print(paste(l,strftime(index(tmp),format="%H:%M:%S",tz="America/New_York")[1]))
      assign(l,tmp)
    }
    
    if (x==4) {
      for (i in 1:length(tmp$data.Date)) {tmp$data.Date[i] <- paste(tmp$data.Date[i],"01","01",sep="")}
      tmp$data.Date <- as.Date(as.character(tmp$data.Date), format="%Y%m%d")
      tmp<-xts(x=tmp[,2],order.by=tmp$data.Date) ; colnames(tmp)<- c(l)
      print(paste(l,strftime(index(tmp),format="%H:%M:%S",tz="America/New_York")[1]))
      assign(l,tmp) 
    }
  }
}

  
  