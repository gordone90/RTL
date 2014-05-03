#################################################
#    Function used to source R functions from Git Hub 
#    This function will prompt you for password to setup R Curl environment for proxy setting 
#    
#
####################################################

# Examples
#source_github("https://gist.github.com/thertrader/7483309/raw/e395a36edbf90b0e5284ecd6c9265e28c5d2142f/quandlDataQuality.R")
#source_github("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R")
sourcegithub <- function(u) {
  # load package
  require(RCurl)
  
  uname<-as.character(Sys.info()['user'])
  passwrd<-.rs.askForPassword("LIM Connect::NT ID Password")
  
  proxypwd_i<-paste("\\SCGLOBAL\\",uname,":",passwrd,sep="")
  options(RCurlOptions = list(proxy="http://proxyprd.scotia-capital.com:8080",proxyuserpwd=proxypwd_i))
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  

