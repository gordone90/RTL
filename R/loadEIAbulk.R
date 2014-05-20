#' \code{loadEIAbulk} 
#' @description Loads all EIA NG and PET data as a bulk download.
#' @return Lots of series...
#' @export loadEIAbulk
#' @author Philippe Cote <coteph@@mac.com,philippe.cote@@scotiabank.com>

loadEIAbulk <- function() {
  eia <-c("NG","PET")
  for (e in eia) {
    temp <- tempfile()
    download.file(paste("http://api.eia.gov/bulk/",e,".zip",sep=""),temp)
    temp<-unzip(temp)
    assign("x",lapply(X=readLines(file(temp,"r"), -1L), fromJSON))
    eiaSeries <- c() ; for (i in 1:length(x)) {eiaSeries <- c(eiaSeries,x[[i]]$series_id)}
    assign(paste("eia",e,"_Names",sep=""),eiaSeries)
    assign(paste("eia",e,sep=""),x)
    rm(x,temp)
  }
}
