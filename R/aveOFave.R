#' \code{aveOFave} 
#' @description Returns a xts object representing the average of multiple averages.
#' @param x Univariate xts series.
#' @param n Vector of averaging period to use.
#' @return A univariate xts object representing the average of averages. 
#' @export aveOFave
#' @author Philippe Cote <coteph@@mac.com>, Nima Safain <nima.safaian@@gmail.com>
#' @examples 
#' data(data)
#' RTL:::aveOFave(x=Cl(CL1),n=c(3,5,10))
aveOFave <- function (x, n = c(3,5,10)) 
{
  x <- na.omit(x)
  numberofave <- length(n)
  x$aveOFave <- 0
  for (i in 1:numberofave) {
    x$aveOFave <- x$aveOFave + (runMean(x[,1], n[i])*1/numberofave)
    }
    x
  return(x$aveOFave)
}