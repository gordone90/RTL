#' \code{plotMultiple} 
#' @description Functionn to automatically size multile object plots. Best used when using a loop to plot a changing number of variables. 
#' @param numberofcharts A numeric value representing the otal number of charts to plots.
#' @param chartsByRow Number of rows you desired in the plot grid.
#' @return Sets par(mfrow=c(chartsByCol,chartsByRow)) 
#' @export plotMultiple
#' @author Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>
#' @examples 
#' data(data)
#' RTL:::plotMultiple(numberofcharts=4,chartsByRow=2)
#' plot(Cl(CL1));plot(Cl(CL2));plot(Cl(CL3));plot(Cl(CL4))
#' RTL:::plotMultiple(numberofcharts=4,chartsByRow=1)
#' plot(Cl(CL1));plot(Cl(CL2));plot(Cl(CL3));plot(Cl(CL4))

plotMultiple <- function(numberofcharts=2,chartsByRow=2) {
  #assign(chartsByRow, 2, envir = .GlobalEnv)
  chartsByCol=numberofcharts/chartsByRow
  par(mfrow=c(chartsByCol,chartsByRow))
  x=matrix(rep(seq(1:(chartsByCol)),1,each=chartsByRow))
  y=matrix(rep(seq(1:chartsByRow),chartsByCol))
  nx=matrix(rep(chartsByCol,numberofcharts))
  ny=matrix(rep(chartsByRow,numberofcharts))
  split_values<-cbind(x,y,nx,ny)
  par(mfrow=c(chartsByCol,chartsByRow))
}