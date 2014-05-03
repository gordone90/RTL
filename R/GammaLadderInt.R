
#####################################################################################################
#  Purpose: Function calculates Does the linear Interpolation but with Better Convexity Calculation
#  Input: X numbers (strikes), Y Numbers Delta, and X which is price move 
#  Output: Gamma + Delta P&L
#  Dependency: approxmatch function
#####################################################################################################

GammaLadderInt<- function (YVector,XVector,X) {

  # Define Variables
  P<-0
  #X<-6
  j<-1
  #K<-1
  P<-1
  # Find where on the ladder the price move is
  S1<-approxmatch(XVector,X)
  # Find the size of the Ladder
  S2<-length(XVector)
  # Flag direction of the move
  if (X > 0) {Xdir<-1} else if (X==0) {Xdir<-0} else {Xdir<-(-1)}
  # Define Interpolation function 
  EInterpolation <- approxfun(XVector, as.vector(YVector))
  # Find mid point of the ladder 
  S1M<-ceiling(S2/2)
  # If prices are moveing up
  if (Xdir == 1) {
  # If price move greater than the ladder mark use ladder values for interpolation  
    for(i in seq(from=S1M,to=S1,by=Xdir)) {    
      if(X>XVector[i+1]) {      
        tmp<-as.vector(YVector[i])*(XVector[i+1]-XVector[i])+0.5*(XVector[i+1]-XVector[i])*(YVector[i+1]-YVector[i])
        P<-c(P,tmp) 
  # If price move falls between ladder marks interpolate      
      } else {
        
        tmp<-as.vector(YVector[i]) * (X - XVector[i]) + 0.5 * (X - XVector[i]) * (EInterpolation(X) - as.vector(YVector[i]))
        P<-c(P,tmp)
        
      }
    }    
  # Use reverse logic for the downward price move  
  } else if (Xdir == -1) {
    
    
    for(i in seq(from=S1M,to=S1,by=Xdir)) {    
      if(X<XVector[i]) {      
        tmp<-as.vector(YVector[i])*(XVector[i-1]-XVector[i])+0.5*(XVector[i-1]-XVector[i])*(as.vector(YVector[i-1])-as.vector(YVector[i]))
        P<-c(P,tmp)     
      } else {        
        tmp<-as.vector(YVector[i]) * (X - XVector[i]) + 0.5 * (X - XVector[i]) * (EInterpolation(X) - as.vector(YVector[i]))
        P<-c(P,tmp)        
      }
    } 
    
  } else {
  #  if no price move return zero P&L
    GammaLadderInt<-0
    
  }   
  # return back the sum of piecewise interpolations
  GammaLadderInt<-sum(P[2:length(P)])
  return(GammaLadderInt)
  
}


