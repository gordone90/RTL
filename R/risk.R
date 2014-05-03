
riskprofile <- function (tmp) {

# purpose: output risk profile of a given time series  
# input: xts object of returns
# output
  #                     at1%  at99%
  # VaR               -0.1112 0.1711
  # sVaR              -1.1660 1.5840
  # Stress_WorstCases -3.0400 9.7500
  
# quantile -> http://en.wikipedia.org/wiki/Quantile
  
  
if (class(tmp)[1]!="xts") stop("Returns time series in NOT an xts class")  
tmp <- na.omit(tmp)

VaR <- last(tmp,n=300)
risk <- data.frame(as.numeric(apply(VaR, 2, quantile, .01, type=8,na.rm=TRUE)),
                   as.numeric(apply(VaR, 2, quantile, .99, type=8,na.rm=TRUE)))
names(risk)<-c("at1%","at99%")

# calculate sVaR
if (nrow(tmp['2008::2009'])==0) {
  junk <- data.frame(0,0)
  names(junk)<-c("at1%","at99%")
  risk <- rbind(risk,junk) ; risk
  
  } else  {
    sVaR <- tmp['2008::2009']  
    junk <- data.frame(as.numeric(apply(sVaR, 2, quantile, type=8, .01, na.rm=TRUE)),
                     as.numeric(apply(sVaR, 2, quantile, type=8, .99, na.rm=TRUE)))
    names(junk)<-c("at1%","at99%")
    risk <- rbind(risk,junk) ; risk
          }

# calculate stress
stress <- tmp
junk <- data.frame(as.numeric(min(stress)),as.numeric(max(stress)))
names(junk)<-c("at1%","at99%")
risk <- rbind(risk,junk)
row.names(risk)<-c("VaR","sVaR","Stress_WorstCases") 

return(risk)
}