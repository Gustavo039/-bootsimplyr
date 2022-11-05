#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @param data: An object of type vector. Refers to the data where the range will be calculated
#' @param theta: An object of type numeric. Refers to the true value of the parameter to be estimated

#' @export
boot_quality=function(data,theta=mean(data)){
  if(!is.numeric(theta))
    stop(" 'theta' argumet must be a numeric value")
  else(is.null(theta))
  bias_=mean(data)-theta
  ####################
  var_data=var(data)
  ###################
  EQM=var_data+bias_**2
  ####################
  df=data.frame(Bias=bias_,Var=var_data,M.S.E=EQM)
  return(df)
}
