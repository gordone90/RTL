#' \code{promptBeta} 
#' @description Returns betas of multiple xts prices (by using relative returns).
#' @param comm Commodity prefix for tickers e.g. "NG", "CL". Refer to fetchdata(). 
#' @param term Suffix of commodity tickers or NULL. Refer to fetchdata().
#' @param period "all" or numeric period of time in days.
#' @param cmdty commodity name in expiry_table to remove returns on roll dates.
#' @param betatype "all" "bull" "bear"
#' @return A list of [1] betas as a dataframe and [2] summary statistics of exponential fit if using term=1:n.
#' @export promptBeta
#' @author Philippe Cote <coteph@@mac.com>, Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::promptBeta(comm="CL",term=3,period="all",cmdty="cmewti",betatype="all") 
#' RTL:::promptBeta(comm=c("CL1","CL2","CL3"),term=NULL,period="all",cmdty="cmewti",betatype="all")

promptBeta<-function(comm,term=NULL,period="all",cmdty="cmewti",betatype="all") {

if(length(term)!=0) {term<-seq(1:term)}
data<-na.omit(fetchdata(comm=comm,term,type="Cl"))

if (is.numeric(period)) {
  last<-nrow(data);first<-last - period
  data<-data[first:last,]
} 

if(comm[1]=="CL") {data['2008-09-22']<-NA ; data<-na.omit(data)}

ret<-data_ret(x=data,returntype=c("relative"))
ret <- na.omit(rolladjust(ret,datatype="returns",commodityname=cmdty,rolltype="Last.Trade"))

all <- CAPM.beta(ret,ret[,1])
bull <- CAPM.beta.bull(ret,ret[,1])
bear <- CAPM.beta.bear(ret,ret[,1])

n<-1:nrow(t(all))
f<-data.frame(Beta=t(all),Prompt=n);names(f)<-c("Beta","Prompt")

betaformula <- "Only applicable when computing term betas and estimating an exponential fit along the term"
if(length(term)!=0) {
  betaformula<-summary(nls(Beta ~ exp(a+b*Prompt),data=f,start=list(a=0,b=0)))
}

out<-cbind(t(all),t(bull),t(bear))
out<-data.frame(out);names(out)<-c("all","bull","bear")

betaout<-list(out=out,betaformula=betaformula)
return(betaout)
}
