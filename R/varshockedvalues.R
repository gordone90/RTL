#' \code{varshockedvalues} 
#' @description Returns B/A VaR shocks rebased on current prices. 
#' @param prices A xts object of prices for the underlying risk factor.
#' @param relshocks A xts object of relative returns.
#' @return A xts object of B/A VaR shocks rebased on last price data.
#' @export varshockedvalues
#' @author Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>
#' @examples 
#' data(data)
#' RTL:::varshockedvalues(prices=Cl(CL1),relshocks=RTL:::data_ret(x=Cl(CL1),returntype="relative"))

varshockedvalues <- function(prices,relshocks) {

  # input: 
  # output: shocked components. relative shock at time t multiplied by current prices
  p<-coredata(last(na.omit(last(prices,n=2))))
  r<-tail(relshocks)
  for (i in 1:ncol(p)) {relshocks[,i] <- as.numeric(p[,i]) * relshocks[,i]}
  return(relshocks)
  
}