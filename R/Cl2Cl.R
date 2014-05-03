Cl2Cl <- function (x1, x2 = NULL, k = 0, type = c("absolute", "percent","percentlog")) 
{
  x1 <- try.xts(x1, error = FALSE)
  type <- match.arg(type[1], c("absolute", "percent","percentlog"))
  if (length(x2) != length(x1) && !is.null(x2)) 
    stop("x1 and x2 must be of same length")
  if (is.null(x2)) {
    x2 <- x1
    if (length(k) < 2) {
      k <- max(1, k)
    }
  }
  dim(x2) <- NULL
  if (type == "percentlog") {
    xx <- lapply(k, function(K.) {
      log(unclass(x2)/Lag(x1, K.))
    })
  }
  if (type == "percent") {
    xx <- lapply(k, function(K.) {
      unclass(x2)/Lag(x1, K.) - 1
    })
  }
  if (type == "absolute") {
    xx <- lapply(k, function(K.) {
      unclass(x2)-Lag(x1, K.) 
    })
  }
  xx <- do.call("cbind", xx)
  colnames(xx) <- paste("Delt", k, type, sep = ".")
  reclass(xx, x1)
}