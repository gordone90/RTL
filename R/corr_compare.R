
corr_compare <- function(ret,n=20) {
  ####### simple corr
  StandardCorr<-runCor(ret[,1],ret[,2], n = n, use = "all.obs", sample = TRUE,cumulative = FALSE)
  names(StandardCorr)<-c("TradintionalCorr")
    
  ####### EWMA ####
  EWMA = matrix(nrow=nrow(ret),ncol=3)  # create a matrix to hold the covariance matrix for each t
  
  lambda = 0.94
  S = cov(ret)  					# initial (t=1) covariance matrix
  
  EWMA[1,] = c(S)[c(1,4,2)]	# extract the variances and covariance
  
  for (i in 2:nrow(ret)){		# loop though the sample
    S = lambda * S  + (1-lambda) * t(ret[i]) %*% ret[i]	
    EWMA[i,] = c(S)[c(1,4,2)]	# convert matrix to vector	
  }
  EWMArho = EWMA[,3]/sqrt(EWMA[,1]*EWMA[,2])	# calculate correlations
  
  ######### OOGarch ######
  #library(gogarch)
  #res = gogarch(ret,formula = ~garch(1,1),garchlist = c(include.mean=FALSE))
  #OOrho = ccor(res)
  
  ######### DCC ######
  library(ccgarch)
  # estimate univatiate GARCH models to get starting values
  f1 = garchFit(~ garch(1,1), data = ret[,1],include.mean=FALSE)
  f1 = f1@fit$coef
  f2 = garchFit(~ garch(1,1), data = ret[,2],include.mean=FALSE)
  f2 = f2@fit$coef
  # create vectors and matrices of starting values
  a = c(f1[1], f2[1]) 
  A = diag(c(f1[2],f2[2]))
  B = diag(c(f1[3], f2[3])) 
  dccpara = c(0.2,0.6) 
  # estimate the model
  dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=ret, model="diagonal")
  # Parameter estimates and their robust standard errors dcc.results$out
  DCCrho = dccresults$DCC[,2]
  
  ######### output results
  corr_compare<-xts(coredata(cbind(EWMArho,DCCrho,coredata(StandardCorr))),order.by=as.Date(index(ret)))
  return(corr_compare)
}

