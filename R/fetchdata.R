fetchdata <- function(comm,term=NULL,type=c("Cl"))
{
  
  if (length(term)!=0) {tmp<-eval(parse(text=paste(type,"(",comm[1],term[1],")",sep="")))} 
  else {tmp <- eval(parse(text=paste(type,"(",comm[1],")",sep="")))}
  
  for (i in 1:length(comm)) {
    if (length(term)!=0) {
      for (j in 1:length(term)) {
      if  (exists(as.character(substitute(eval(parse(text=paste(type,"(",comm[i],term[j],")",sep=""))))))==FALSE) stop(paste(type,"(",comm[i],term[j],")",sep=""))
        
      tmp1 <- eval(parse(text=paste(type,"(",comm[i],term[j],")",sep=""))) ; names(tmp1)<-paste(comm[i],term[j],sep="") ; head(tmp1)
      tmp <- merge(tmp,tmp1) 
      }
    }
    else {
      tmp1 <- eval(parse(text=paste(type,"(",comm[i],")",sep=""))) ; names(tmp1)<-paste(comm[i],sep="") ; head(tmp1)
      tmp <- merge(tmp,tmp1)
    }
  }
  #assign(eval(parse(text=paste("tmp$",names(tmp)[1],sep=""))),NULL)
  #tmp$Close <- NULL 
  tmp <- tmp[,-1]
  tmp
      
}

