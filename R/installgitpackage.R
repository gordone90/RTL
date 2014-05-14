#' \code{installgitpackage} 
#' @description Installs packages from Github
#' @param proxy Logical value. TRUE is proxy is required.
#' @param proxyhttp If proxy=TRUE takes the following "http://..."
#' @param packagename Name of package to be loaded from Github
#' @param gitrepo Name of Github repository
#' @return Installs Github package
#' @export installgitpackage
#' @author Philippe Cote <coteph@@mac.com>, Nima Safaian <nima.safaian@@gmail.com>
#' @examples 
#' RTL:::installgitpackage(proxy=FALSE,proxyhttp=NULL,packagename="rCharts",gitrepo="ramnathv")
installgitpackage <- function(proxy=TRUE,proxyhttp=NULL,packagename="RTL",gitrepo="risktoollib") {
  local({r <- getOption("repos")
         r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})
  install.packages(c("httr","devtools"),type='source')
  require("devtools")
  require("httr")
  if (proxy==TRUE) {set_config(use_proxy(url=proxyhttp,port=8080,username=readline("enter network username:"),password=readline("enter password:")))}
  devtools::install_github(packagename,gitrepo)
}