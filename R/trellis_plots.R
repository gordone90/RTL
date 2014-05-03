trellis_plots <- function(numberofcharts=2,chartsByRow=2) {
  cnt<-numberofcharts
  chartsByRow=2 ; assign(chartsByRow, 2, envir = .GlobalEnv)
  chartsByCol=cnt/chartsByRow
  par(mfrow=c(chartsByCol,chartsByRow))
  x=matrix(rep(seq(1:(chartsByCol)),1,each=chartsByRow))
  y=matrix(rep(seq(1:chartsByRow),chartsByCol))
  nx=matrix(rep(chartsByCol,cnt))
  ny=matrix(rep(chartsByRow,cnt))
  split_values<-cbind(x,y,nx,ny)
  par(mfrow=c(chartsByCol,chartsByRow))
}