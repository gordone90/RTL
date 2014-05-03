# Description: Converts Numeric Month to Character month and reverse
month2char<-function(i) {
  
  m2c<-data.frame(m=c(1,2,3,4,5,6,7,8,9,10,11,12),
                  c=c("F","G","H","J","K","M","N","Q","U","V","X","Z"))    
 
  if(is.numeric(i)) {  
    return(as.character(subset(m2c,m %in% i)$c))
  } else {  
    return(subset(m2c,c %in% i)$m)    
  }  

} 


