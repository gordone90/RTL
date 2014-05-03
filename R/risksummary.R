risksummary <- function(series1,series2,commodityname,rolltype,roll=TRUE) {
  #### SERIES 1 is the long position, SERIES 2 is the short position
  prices <- merge(series1,series2)
  spd <- series1-series2 ; spd <- na.omit(spd)
  
  # compute risk based on absolute returns 
  ret_abs <- data_ret(spd)[[1]]
  if (roll==TRUE){ret_abs_adj <- rolladjust(ret_abs,commodityname,type=rolltype)} else {ret_abs_adj <- data_ret(spd)[[1]]}
  returns_absolute <- merge(ret_abs,ret_abs_adj) ; names(returns_absolute)<-c("ret_abs","ret_abs_adj")
  xyplot(returns_absolute,main="Returns Pre and Post Roll-Adjustment")  
  risk_abs <- list(print("risk based on absulute return: pre and post roll adjusted"),print(riskprofile(ret_abs)),print(riskprofile(ret_abs_adj)))
  
  # risk based on relative returns
  ret_rel_raw <- data_ret(prices)[[2]] # gives returns 1.05 -1 = 0.05
  retrel<-varshockedvalues(prices,ret_rel_raw) # returns $value of relative shocks applied to current prices
  ret_rel <- retrel[,1] - retrel[,2] ; names(ret_rel)<-c("spd") # calculate P&L vector of the spread 
  if (roll==TRUE){ret_rel_adj <- rolladjust(ret_rel,commodityname,type=rolltype)} else {ret_rel_adj <- ret_rel}
  #ret_rel_adj <- rolladjust(ret_rel,commodityname,type=rolltype)
  returns_relative <- merge(ret_rel,ret_rel_adj) ; names(returns_relative)<-c("ret_rel","ret_rel_adj")
  xyplot(returns_relative,main="Returns Pre and Post Roll-Adjustment")  
  risk_rel <- list(print("risk based on relative return: pre and post roll adjusted"),print(riskprofile(ret_rel)),print(riskprofile(ret_rel_adj)))

  out<-list(risk_abs,risk_rel,returns_abs=returns_absolute,returns_rel=returns_relative)
  return(out)
  
} 
