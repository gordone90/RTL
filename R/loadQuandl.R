#' \code{loadQuandl} 
#' @description Returns a xts object representing the average of multiple averages.
#' @param tickers Dataframe in the same format as the tickers data object in data(data). 
#' @param token Quandl token.
#' @param start_date Start date as character yyy-mm-dd.
#' @param end_date Start date as character yyy-mm-dd.
#' @return A univariate xts object for each tickers. 
#' @export loadQuandl
#' @author Nima Safain <nima.safaian@@gmail.com,nima.safaian@@scotiabank.com>, Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>
#' @examples 
#' data(data)
#' RTL:::loadQuandl(tickers=subset(tickers,source %in% "quandl")[10,],token="CQzMpZ5DKPkekYQiXSHM",start_date="2003-01-01", end_date=format(Sys.Date(),"%Y-%m-%d"))

loadQuandl <- function(tickers=subset(tickers,source %in% "quandl"),token="yourtoken",start_date="2003-01-01", end_date=format(Sys.Date(),"%Y-%m-%d")) {
  Quandl.auth(token)
  for (i in 1:nrow(tickers)) {assign(as.character(tickers[i,2]),Quandl(as.character(tickers[i,1]),type="xts",start_date=start_date,end_date=end_date),envir = as.environment(1))} 
  }


