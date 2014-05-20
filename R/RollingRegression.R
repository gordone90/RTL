#' \code{RollingRegression} 
#' @description Returns an xts object of the rolling regression attribute selected.
#' @param Ra A univariate xts object of returns.
#' @param Rb A univariate or multivariate xts object of returns.
#' @param width Number of periods to compute correlation over. 
#' @param Rf Risk-free rate. Zero by default.
#' @param attribute Any of "Beta", "Alpha" or "R-Squared".
#' @param main set the chart title, same as in plot.
#' @param na.pad TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped.
#' @param ... any other passthru parameters.
#' @return A matrix of results. 
#' @export RollingRegression
#' @references Amended version of PerformanceAnalytics package chart.RollingRegression to return data instead of a chart.
#' @author Nima Safain <nima.safaian@@gmail.com,nima.safaian@@scotiabank.com>
#' @examples
#' data(data)
#' Ra<-RTL:::data_ret(x=Cl(CL1),returntype=c("relative"))
#' Rb<-RTL:::data_ret(x=(CL2),returntype=c("relative"))
#' RTL:::RollingRegression(Ra=Ra, Rb=Rb, width = 20,attribute = c("Beta", "Alpha", "R-Squared"))

RollingRegression <- function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main = NULL, na.pad = TRUE, ...) 
{
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
