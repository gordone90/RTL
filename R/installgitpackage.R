installgitpackage <- function(proxy=TRUE,proxyhttp=NULL,packagename="RTL",gitrepo="risktoollib") {
  install.packages(c("httr","devtools"),type='source')
  require("devtools")
  require("httr")
  if (proxy==TRUE) {set_config(use_proxy(url=proxyhttp,port=8080,username=readline("enter network username:"),password=readline("enter password:")))}
  devtools::install_github(packagename,gitrepo)
}