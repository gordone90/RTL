#' \code{RollingRegression} 
#' @description Returns an xts object of the rolling regression attribute selected.
#' @param Ra A univariate xts object of returns.
#' @param Rb A univariate or multivariate xts object of returns.
#' @param width Number of periods to compute correlation over. 
#' @param Rf Risk-free rate. Zero by default.
#' @param attribute Any of "Beta", "Alpha" or "R-Squared".
#' @param ... any other passthru parameters.
#' @return A matrix of results. 
#' @export RollingRegression
#' @author Philippe Cote <coteph@@mac.com>, Nima Safain <nima.safaian@@gmail.com>
#' @examples
#' data(data)
#' Ra<-RTL:::data_ret(x=Cl(CL1),returntype=c("relative"))
#' Rb<-RTL:::data_ret(x=(CL2),returntype=c("relative"))
#' RTL:::RollingRegression(Ra=Ra, Rb=Rb, width = 20,attribute = c("Beta", "Alpha", "R-Squared"))

RollingRegression <- function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main = NULL, na.pad = TRUE, ...) 
{
  if (!require("quantreg", quietly = TRUE)) 
    stop("package", sQuote("quantreg"), "is needed.  Stopping")
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  attribute = attribute[1]
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  Ra.excess = Return.excess(Ra, Rf)
  Rb.excess = Return.excess(Rb, Rf)
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra.excess[, column.a, drop = FALSE], 
                            Rb.excess[, column.b, drop = FALSE])
      if (attribute == "Alpha") 
        column.result = rollapply(na.omit(merged.assets[, 
                                                        , drop = FALSE]), width = width, FUN = function(x) rq(x[, 
                                                                                                                1, drop = FALSE] ~ x[, 2, drop = FALSE])$coefficients[1], 
                                  by = 1, by.column = FALSE, fill = if (na.pad) 
                                    NA, align = "right")
      if (attribute == "Beta") 
        column.result = rollapply(na.omit(merged.assets[, 
                                                        , drop = FALSE]), width = width, FUN = function(x) rq(x[, 
                                                                                                                1, drop = FALSE] ~ x[, 2, drop = FALSE])$coefficients[2], 
                                  by = 1, by.column = FALSE, fill = if (na.pad) 
                                    NA, align = "right")
      if (attribute == "R-Squared") 
        column.result = rollapply(na.omit(merged.assets[, 
                                                        , drop = FALSE]), width = width, FUN = function(x) summary(rq(x[, 
                                                                                                                        1, drop = FALSE] ~ x[, 2, drop = FALSE]))$r.squared, 
                                  by = 1, by.column = FALSE, fill = if (na.pad) 
                                    NA, align = "right")
      column.result.tmp = xts(column.result)
      colnames(column.result.tmp) = paste(columnnames.a[column.a], 
                                          columnnames.b[column.b], sep = " to ")
      column.result = xts(column.result.tmp, order.by = time(column.result))
      if (column.a == 1 & column.b == 1) 
        Result.calc = column.result
      else Result.calc = merge(Result.calc, column.result)
    }
  }
  if (is.null(main)) {
    freq = periodicity(Ra)
    switch(freq$scale, minute = {
      freq.lab = "minute"
    }, hourly = {
      freq.lab = "hour"
    }, daily = {
      freq.lab = "day"
    }, weekly = {
      freq.lab = "week"
    }, monthly = {
      freq.lab = "month"
    }, quarterly = {
      freq.lab = "quarter"
    }, yearly = {
      freq.lab = "year"
    })
    main = paste("Rolling", freq.lab, attribute, sep = " ")
  }
  #  chart.TimeSeries(Result.calc, main = main, ...)
  return(Result.calc)
}
