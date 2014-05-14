heatmap2 <- function(data,x,y,z,title="",set.range=F,ShowminORmax="max",mypanel=F) {
#set.range=T if you want the scale to be -x,+x or False uses actual min and actual max
#ShowminORmax= "min" to output min values in the title, "max" to show max values
#mypanel=T to show values in the cells or F just to show color
  #library(RColorBrewer)
  colorrange=colorRampPalette(brewer.pal(11, "Spectral"))
  myPanel <- function(x, y, z, ...) {panel.levelplot(x,y,z,...); panel.text(x, y, round(z,1))}

  if(ShowminORmax =="max") {tmp<-subset(data,data[[z]] %in% max(data[[z]]))} else {tmp<-subset(data,data[[z]] %in% min(data[[z]]))}
 
  if (set.range==T) {
    upper<-max(data[[z]])*1.1
    lower<-min(data[[z]])*0.9
    range<-max(abs(upper),abs(lower))
    zrange=seq(-range,range,range*2/100)
    if(mypanel) {levelplot(data[[z]] ~  data[[x]]*data[[y]],main=ifelse(title=="",z,paste(z,"for",title,"\n",x,"=",tmp[[x]],y,"=",tmp[[y]],z,"=",round(tmp[[z]],2))),xlab=x,ylab=y,region = TRUE, aspect = "fill",at=zrange,col.regions = colorrange,cex.lab=3)
    } else {    levelplot(data[[z]] ~  data[[x]]*data[[y]],main=ifelse(title=="",z,paste(z,"for",title,"\n",x,"=",tmp[[x]],y,"=",tmp[[y]],z,"=",round(tmp[[z]],2))),xlab=x,ylab=y,region = TRUE, aspect = "fill",at=zrange,col.regions = colorrange,cex.lab=3,panel=myPanel)
}
    } else {
      
      if(mypanel) {levelplot(data[[z]] ~  data[[x]]*data[[y]],main=ifelse(title=="",z,paste(z,"for",title,"\n",x,"=",tmp[[x]],y,"=",tmp[[y]],z,"=",round(tmp[[z]],2))),xlab=x,ylab=y,region = TRUE, aspect = "fill",col.regions = colorrange,cex.lab=3,panel=myPanel)
      } else {levelplot(data[[z]] ~  data[[x]]*data[[y]],main=ifelse(title=="",z,paste(z,"for",title,"\n",x,"=",tmp[[x]],y,"=",tmp[[y]],z,"=",round(tmp[[z]],2))),xlab=x,ylab=y,region = TRUE, aspect = "fill",col.regions = colorrange,cex.lab=3)
      }
    
  }
}


