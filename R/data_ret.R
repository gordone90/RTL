#' \code{data_ret} 
#' @description Returns a xts object of absolute or relative returns.
#' @param x Univariate or multivariate xts object.
#' @param returntype Define return type: accepts "absolute" or "relative" as input.
#' @return A univariate or multivariate xts object.
#' @export data_ret
#' @author Philippe Cote <coteph@@mac.com>, Nima Safain <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::data_ret(x=CL1,returntype=c("absolute"))

data_ret <- function (x,returntype=c("absolute")) {
  x <- na.omit(x)
  if (!returntype %in% c("absolute","relative")) {print("invalid return type");break}
  data_ret_abs <- (x-lag(x,lag=1,arithmetic=FALSE)) ; data_ret_abs <- na.omit(data_ret_abs)
  data_ret_rel <- (x/lag(x,lag=1,arithmetic=FALSE))-1 ; data_ret_rel <- na.omit(data_ret_rel)
  if (returntype=="absolute") {return(data_ret_abs)}
  if (returntype=="relative") {return(data_ret_abs)}
}