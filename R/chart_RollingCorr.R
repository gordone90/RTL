#' \code{chart.RollingCorr} 
#' @description chart.RollingCorrelation from PerformanceAnalytics using Spearman method and customized ylim
#' @param Ra A univariate xts object of returns.
#' @param Rb A univariate or multivariate xts object of returns.
#' @param width Number of periods to compute correlation over. 
#' @param legend.loc places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center.
#' @param xaxis If true, draws the x axis
#' @param colorset Color palette to use, set by default to rational choices
#' @param ... any other passthru parameters
#' @param ylimmin ylim minimum value
#' @param ylimmax ylim maximum value
#' @param fill a three-component vector or list (recycled otherwise) providing filling values at the left/within/to the right of the data range. See the fill argument of na.fill for details.
#' @return A univariate xts object representing the average of averages. 
#' @export chart.RollingCorr
#' @author Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>, Nima Safain <nima.safaian@@gmail.com,nima.safaian@@scotiabank.com>
#' @examples
#' data(data)
#' Ra<-RTL:::data_ret(x=Cl(CL1),returntype=c("relative"))
#' Rb<-RTL:::data_ret(x=(CL2),returntype=c("relative"))
#' chart.RollingCorr<-function (Ra=Ra, Rb=Rb, width = 12, xaxis = TRUE, legend.loc = NULL,colorset = (1:12), ylimmin=-1,ylimmax=1,..., fill = NA)

chart.RollingCorr<-function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL,colorset = (1:12), ylimmin=-1,ylimmax=1,..., fill = NA) 
{
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra[, column.a, drop = FALSE], 
                            Rb[, column.b, drop = FALSE])
      column.calc = rollapply(na.omit(merged.assets[, , 
                                                    drop = FALSE]), width = width, FUN = function(x) cor(x[,1, drop = FALSE], x[, 2, drop = FALSE],method = "spearman"), by = 1, 
                              by.column = FALSE, fill = fill, align = "right")
      column.calc.tmp = xts(column.calc)
      colnames(column.calc.tmp) = paste(columnnames.a[column.a], 
                                        columnnames.b[column.b], sep = " to ")
      column.calc = xts(column.calc.tmp, order.by = time(column.calc))
      if (column.a == 1 & column.b == 1) 
        Result.calc = column.calc
      else Result.calc = merge(Result.calc, column.calc)
    }
  }
  chart.TimeSeries(Result.calc, xaxis = xaxis, colorset = colorset,legend.loc = legend.loc, ylim = c(ylimmin, ylimmax), ...)
}