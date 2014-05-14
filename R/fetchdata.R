#' \code{fetchdata} 
#' @description Returns a xts object of absolute or relative returns.
#' @param comm Prefix of xts objects to form a multivariate xts object from or character vector of xts objects to merge.
#' @param term If using comm prefix, a numeric vector of suffixes to combine.
#' @param type if xts objects are multivariate OHLCV objects, select which OHLCV column to use using quantmod functions (Cl(),Hi(),Lo(),Op(),Vo())
#' @return A multivariate xts object.
#' @export fetchdata
#' @author Philippe Cote <coteph@@mac.com>, Nima Safain <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::fetchdata(comm="CL",term=c(1,2),type=c("Cl"))
#' RTL:::fetchdata(comm=c("CL1","CL2"),term=NULL,type=c("Cl"))

fetchdata <- function(comm,term=NULL,type=c("Cl"))
{
  if (length(term)!=0) {tmp<-eval(parse(text=paste(type,"(",comm[1],term[1],")",sep="")))} 
  else {tmp <- eval(parse(text=paste(type,"(",comm[1],")",sep="")))}
  
  for (i in 1:length(comm)) {
    if (length(term)!=0) {
      for (j in 1:length(term)) {
      if  (exists(as.character(substitute(eval(parse(text=paste(type,"(",comm[i],term[j],")",sep=""))))))==FALSE) stop(paste(type,"(",comm[i],term[j],")",sep=""))
      tmp1 <- eval(parse(text=paste(type,"(",comm[i],term[j],")",sep="")))
      names(tmp1)<-paste(comm[i],term[j],sep="")
      tmp <- merge(tmp,tmp1) 
      }
    } else {
      tmp1 <- eval(parse(text=paste(type,"(",comm[i],")",sep="")))
      names(tmp1)<-paste(comm[i],sep="")
      tmp <- merge(tmp,tmp1)
    }
  }
  tmp <- tmp[,-1]
  na.omit(tmp)
}

