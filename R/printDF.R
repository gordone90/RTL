printDF <- function(table,title,footnote,fontsize=12) {
  table<-tableGrob(table)
  h <- grobHeight(table)
  w <- grobWidth(table)
  title<-textGrob(title, y=unit(0.5,"npc") + 0.5*h,vjust=0, gp=gpar(fontsize=fontsize,col="blue",fontface="bold"))
 footnote <- textGrob(footnote,
                     x=unit(0.5,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))
  grid.draw(gTree(children=gList(table,title,footnote)))
}