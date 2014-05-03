data_ret <- function (tmp) {
  
  # purpose: adjust returns for roll  
  # input: xts object of returns, commodity name
  # output: roll adjusted xts object of returns data_ret[1] = abs data_ret[2] = rel
  tmp <- na.omit(tmp)
  data_ret_abs <- (tmp-lag(tmp,lag=1,arithmetic=FALSE)) ; data_ret_abs <- na.omit(data_ret_abs)
  data_ret_rel <- (tmp/lag(tmp,lag=1,arithmetic=FALSE))-1 ; data_ret_rel <- na.omit(data_ret_rel)

  return(list(data_ret_abs,data_ret_rel))
}