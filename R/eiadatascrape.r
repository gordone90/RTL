#EIAScrape("PET.MCRFPUS1.M",api_key)
EIAScrape<-function(seriesid,api_key="83307AB94D4FC78A247EF9E5FAE67581") {
  #library(RJSONIO)
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