chart_spreads<- function(from=2011,to=2012,contract="NG",month1=10,month2=11,comdty="cmeng",daysfromexpiry=365,legendpos="topleft") {
  Sys.setenv(TZ="UTC")
  span <- seq(from,to,by=1)

  if (month1==1) {m1="F"} ; if (month2==1) {m2="F"}
  if (month1==2) {m1="G"} ; if (month2==2) {m2="G"}
  if (month1==3) {m1="H"} ; if (month2==3) {m2="H"}
  if (month1==4) {m1="J"} ; if (month2==4) {m2="J"}
  if (month1==5) {m1="K"} ; if (month2==5) {m2="K"}
  if (month1==6) {m1="M"} ; if (month2==6) {m2="M"}
  if (month1==7) {m1="N"} ; if (month2==7) {m2="N"}
  if (month1==8) {m1="Q"} ; if (month2==8) {m2="Q"}
  if (month1==9) {m1="U"} ; if (month2==9) {m2="U"}
  if (month1==10) {m1="V"} ; if (month2==10) {m2="V"}
  if (month1==11) {m1="X"} ; if (month2==11) {m2="X"}
  if (month1==12) {m1="Z"} ; if (month2==12) {m2="Z"}
  
  for (year in span) {
      c1<-get(paste(contract,"_",year,m1,sep=""))
      c2<-get(paste(contract,"_",if (month1<month2) {year} else {year+1},m2,sep=""))
      spd=c1-c2 ; names(spd)=c(as.character(year))
      expiry=as.POSIXct(closestDt(as.POSIXct(paste(year,if(month1<10){0},month1,"01",sep=""),format="%Y%m%d",tz="America/New_York")
                                  ,subset(expiry_table,cmdty==comdty, select= Last.Trade),roundDown=TRUE),format="%Y%m%d",tz="America/New_York")
      
     # expiry>last(index(spd));(expiry-last(index(spd)))
      if (expiry<=last(index(spd))) {spd$daysfromexpiry <- ((index(spd)-last(index(spd)))/24/60/60) } else {spd$daysfromexpiry <- (index(spd)-expiry) }
      #spd$daysfromexpiry <- (index(spd)-  (if(expiry<last(index(spd))){last(index(spd))} else{expiry} )  )/24/60/60
      
      spd<-as.data.frame(coredata(spd)); spd<- spd[,c("daysfromexpiry",paste("X",as.character(year),sep=""))]
      spd<-as.data.frame(spd)
     if (year==span[1]) {mat <- spd } #; names(mat)<-c("DayfromExpiry",as.character(year))}
      if (year!=span[1]) {
        if(last(spd[,1])<last(mat[,1])) {b<-mat[mat[,1]>last(spd[,1]),1:2] ;b[,2]<-NA ; names(b)[2]<-names(spd)[2] ; spd<- rbind(spd,b)}
        mat <- merge(mat,spd,by=1,all=TRUE)
      }
  }
  mat<- mat[mat$daysfromexpiry>-daysfromexpiry,]
  n=length(span)+1
  col = c("blue","red","black","orange","cornflowerblue","darkmagenta","gray48","darkviolet") ; col=col[1:n-1]
  lwd=seq(1,3,by=2/(n-1))
  lty=c("solid", "dashed", "dotted", "dotdash", "longdash","twodash","solid", "dashed"); lty=lty[1:n-1]
  #mat<-last(mat,daysfromexpiry)
  matplot(mat[,1],mat[,2:n],type="l",main=paste(contract,m1,"vs",m2),xlab="Calendar Days from Expiry",ylab="",col=col,lwd=lwd,lty=lty) 
  legend(paste(sep="",legendpos),names(mat)[2:n],lwd=lwd,lty=lty,col=col) ; grid()
  # clean up workspace
  Sys.setenv(TZ="America/New_York")
}

closestDt <- function(searchDate, dateList, roundDown=FALSE) {as.POSIXct(if( roundDown ){max( dateList[ dateList <= as.character(searchDate) ] ) } else {min( dateList[ dateList >= as.character(searchDate)])},,tz="America/New_York")}

