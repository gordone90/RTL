#' \code{riskprofile} 
#' @description Computes empirical VaR, sVaR and Stress Worst Cases from a returns xts object. 
#' @param x An xts object of returns.
#' @return A matrix of results
#' @export riskprofile
#' @author Philippe Cote <coteph@@mac.com>, Nima Safain <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::riskprofile(x=RTL:::data_ret(x=Cl(CL1),returntype="relative"))
#' RTL:::riskprofile(x=RTL:::data_ret(x=Cl(CL1),returntype="absolute"))

riskprofile <- function (x) {
  
if (class(x)[1]!="xts") stop("Returns time series in NOT an xts class")  
x <- na.omit(x)

VaR <- last(x,n=300)
risk <- data.frame(as.numeric(apply(VaR, 2, quantile, .01, type=8,na.rm=TRUE)),
                   as.numeric(apply(VaR, 2, quantile, .99, type=8,na.rm=TRUE)))
names(risk)<-c("at_1_Percent","at_99_Percent")

# calculate sVaR
if (nrow(x['2008::2009'])==0) {
  junk <- data.frame(0,0)
  names(junk)<-c("at_1_Percent","at_99_Percent")
  risk <- rbind(risk,junk) ; risk
  
  } else  {
    sVaR <- x['2008::2009']  
    junk <- data.frame(as.numeric(apply(sVaR, 2, quantile, type=8, .01, na.rm=TRUE)),
                     as.numeric(apply(sVaR, 2, quantile, type=8, .99, na.rm=TRUE)))
    names(junk)<-c("at_1_Percent","at_99_Percent")
    risk <- rbind(risk,junk) ; risk
          }

# calculate stress
stress <- x
junk <- data.frame(as.numeric(min(stress)),as.numeric(max(stress)))
names(junk)<-c("at_1_Percent","at_99_Percent")
risk <- rbind(risk,junk)
row.names(risk)<-c("VaR","sVaR","Stress_WorstCases") 

return(risk)
}