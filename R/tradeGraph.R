tradeGraphs_pc <- function (stats, free.params, params.filter = NULL, statistics, 
          title = NULL) 
{
  if (!require(rgl, quietly = TRUE)) 
    stop("The \"rgl\" package is required to use this function")
  if (!require(reshape2, quietly = TRUE)) 
    stop("The \"reshape2\" package is required to use this function")
  if (missing(stats)) 
    stop("stats undefined")
  if (missing(free.params)) 
    stop("free.params undefined")
  if (length(free.params) != 2) 
    stop("free.params must be a vector of length 2")
  if (missing(statistics)) 
    stop("must specify at least one statistics column to draw graph")
  var1 <- free.params[1]
  var2 <- free.params[2]
  for (statistic in statistics) {
    var3 <- statistic
    if (length(params.filter) == 0) {
      data <- stats[, c(var1, var2, var3)]
    }
    else {
      data <- subset(stats, eval(parse(text = params.filter)), 
                     select = c(var1, var2, var3))
    }
    x <- recast(data, as.formula(paste0(var1, " ~ ", var2)), 
                id.var = c(var1, var2), measure.var = c(var3))$labels[[1]][, 
                                                                           1]
    y <- recast(data, as.formula(paste0(var1, " ~ ", var2)), 
                id.var = c(var1, var2), measure.var = c(var3))$labels[[2]][, 
                                                                           1]
    z <- recast(data, as.formula(paste0(var1, " ~ ", var2)), 
                id.var = c(var1, var2), measure.var = c(var3))$data
    col <- heat.colors(length(z))[rank(z)]
    open3d()
    persp3d(x, y, z, color = col, xlab = var1, ylab = var2, 
            zlab = var3, main = title)
  }
}