# S1=4.788 ; sigma1=0.295 ; S2=4.996 ; sigma2=0.295 ; rho=0.70 ; price=0.15 ; X=0.2

solverCSO <- function(S1,S2,sigma1,sigma2,X,price,expiry="2014-02-26",date="2014-01-01") {
  tmp<-data.frame(corr=seq(-1,1,by=0.001),diff=1)
  for (i in 1:nrow(tmp)) 
    {
  tmp$diff[i]<-abs(SpreadApproxOption(TypeFlag = "c", S1 = S1, S2 = S2, X = X, Time = as.numeric((as.Date(expiry)-as.Date(date))/365), r = 0.0025, sigma1 = sigma1, sigma2 = sigma2, rho = tmp$corr[i])@price - price)
    }
  tmp$corr[tmp$diff==min(tmp$diff)]
  
}

# solverCSO(S1,S2,sigma1,sigma2,X=X,price,expiry="2014-02-26",date="2014-01-01")

