#' \code{chart_Seasonal} 
#' @description Seasonal plot 
#' @param x A univariate xts object to plot. The colnames() will be included in the chart title.
#' @param start_year A numeric YYYY start year from the xts object. 
#' @param end_year A numeric YYYY end year from the xts object. 
#' @param output "chart" for trellis object or "data" for data frame of values. 
#' @return A trellis object if output="chart" or a dataframe if output="data".
#' @export chart_Seasonal
#' @author Gordon Evens <gordon.evens@@me.com,gordon.evens@@scotiabank.com>,Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>
#' @examples
#' data(data)
#' x=Cl(CL1) ; colnames(x) <- c("WTI")
#' RTL:::chart_Seasonal(x=x,start_year=2010,end_year=year(Sys.Date()),output="data")
#' RTL:::chart_Seasonal(x=x,start_year=2010,end_year=year(Sys.Date()),output="chart")

chart_Seasonal <- function(x=Cl(CL1), start_year=2011, end_year=year(Sys.Date()),output="chart") {
  
  #Determine the amount of years that will be plotted
  years_count <- end_year - start_year+1
  year_list <- seq(start_year, end_year, 1)
  cols_needed = years_count+6
  year_mat <- matrix(, nrow= 366, ncol = cols_needed)
  
  #Create generic date matrix for placeholders
  date_array <- seq(from=as.Date('2000-01-01'),to=as.Date('2000-12-31'), by ='days')
  
  #Set up month and day column in Master Matrix
  for (i in 1:length(date_array)){
    year_mat[i,1]<- format(date_array[i], "%b-%d")
    year_mat[i,2] <- format(date_array[i], "%m")
    year_mat[i,3] <- format(date_array[i], "%d")
  }
  
  #Delcare data in yearly arrays before manipulating
  for (a in 1:years_count){
    date_call <- paste0(year_list[a], "-01-01/",year_list[a],"-12-31", seq="")
    assign((paste("y",1, sep="")), x[date_call])
    x_spot <-1
    
    for (i in 1: nrow(y1))
    {
      #Align index for months and days
      adj_mon <- .indexmon(y1[i,])+1
      adj_day <- .indexmday(y1[i,])
      yy1.matrix = data.matrix(as.data.frame(y1))
      col <- a +3
      spot_x <-1
      
      #-------Text Cast Bullshiit ---------------------------
      if(adj_mon <10){adj_mon_txt <- paste0("0",toString(adj_mon), sep="")} else {adj_mon_txt <- toString(adj_mon)}
      if(adj_day < 10 ){adj_day_txt <- paste0("0", toString(adj_day), sep="")} else {adj_day_txt <- toString(adj_day)}
      #---------------------------------------------------
      
      for (j in spot_x:366){
        if(year_mat[j,2]== adj_mon_txt){ 
          if(year_mat[j,3]==adj_day_txt){
            num_data <- as.numeric(yy1.matrix[i])
            year_mat[j,col]<- as.numeric(num_data)
            x_spot <- j
            break
          }
        }
      }
    }
    
    yyy <- as.numeric(year_mat[,col])
    
    if(a == 1){
      DF <- data.frame(yyy)
    }else{
      DF <- data.frame(yyy, DF)
    }
  }
  
  DF <- data.frame(seq(1:366), DF)
  names(DF) <- c("time",rev(year_list))
  time_d <- seq(as.Date("2014-01-01"), by=1, len =366)
  DF$time <- NULL
  DFX <- xts(DF, time_d)
  data <- na.approx(DFX)
  chart<-xyplot(data, 
         screens = 1, 
         col=1:years_count, 
         type = c("l", "g"), 
         lwd= 2,
         xlab = "Month", 
         ylab = "Closing Value", 
         auto.key = list(space= "right", points = TRUE, lines = F),
         main = paste("Seasonality Plot for",colnames(x)[1])
  )
  if (output=="chart") {return(plot(chart))}
  if (output=="data") {return(data)}
}