
PromptBeta<-function(icomm,term=40,period="all",cmdty="cmewti",betatype="all") {
  # INPUTs
    # icomm = cmdty prefix in session e.g. "NG", "CL"
    # last = "all" or last= specify number of days
    #betatype = "all" "bull" "bear"
  #OUTPUT
    # list of [1] betas as adataframe and [2] summary statistics
  
  # dependencies
  func <- c("fetchdata","data_ret","rolladjust")
  for (f in func) { if (!exists(f)) {stop(print(paste("need to load function",f)))}}

term<-seq(1:term)
data<-na.omit(fetchdata(comm=icomm,term,type="Cl"))

if (is.numeric(period)) {
  last<-nrow(data);first<-last - period
  data<-data[first:last,]
} 

if(icomm=="CL") {data['2008-09-22']<-NA ; data<-na.omit(data)}

ret<-data_ret(data)[[1]]
ret <- na.omit(rolladjust(ret,datatype="returns",commodityname=cmdty,rolltype="Last.Trade"))

all <- CAPM.beta(ret,ret[,1])
bull <- CAPM.beta.bull(ret,ret[,1])
bear <- CAPM.beta.bear(ret,ret[,1])

n<-1:nrow(t(all))
f<-data.frame(Beta=t(all),Prompt=n);names(f)<-c("Beta","Prompt")
betaformula<-nls(Beta ~ exp(a+b*Prompt),data=f,start=list(a=0,b=0))

out<-cbind(t(all),t(bull),t(bear))
out<-data.frame(out);names(out)<-c("all","bull","bear")

betaout<-list(out=out,betaformula=summary(betaformula))
#
#   Make it more generic
#

return(betaout)

}
