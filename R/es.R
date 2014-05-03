es <- function(x,ci) {
  x<-coredata(x) ; x<-na.omit(x) ; x<-sort(x)
  cnt<-length(x) ; cnt
  r<-round(ci*cnt)
  if (ci>.5) {es<-mean(x[r:cnt])} else {es<-mean(x[1:r])}
}