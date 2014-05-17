#' \code{EIAscrape} 
#' @description Fetches raw data from EIA API website.
#' @param seriesid EIA series name. 
#' @param api_key EIA API token.
#' @return A list of two objects: name as character and data as a data frame.
#' @export EIAscrape
#' @author Philippe Cote <coteph@@mac.com>, Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::EIAscrape(seriesid="PET.MCRFPUS1.M",api_key="83307AB94D4FC78A247EF9E5FAE67581")

EIAscrape<-function(seriesid,api_key=api_key) {
  series_id<-seriesid
  query<-paste("http://api.eia.gov/series?api_key=",api_key,"&","series_id=",series_id,"&out=json",sep="")
  yy<-fromJSON(query)
  seriesname<-yy$series[[1]]$description;seriesname<-gsub(" ","",seriesname);seriesname<-gsub("\\.","",seriesname);
  seriesunit<-yy$series[[1]]$units;seriesunit<-gsub(" ","",seriesunit);seriesunit<-gsub("\\.","",seriesunit);
  data_tmp <-do.call(rbind,yy$series[[1]]$data)
  names(data_tmp)<-c("Date",seriesunit)              
  out<-data.frame(year=as.numeric(data_tmp[,1]),value=as.numeric(data_tmp[,2]))
  names(out)<-c("Date",seriesunit)              
  out<-list(name=seriesname,data=out)
  return(out)
}