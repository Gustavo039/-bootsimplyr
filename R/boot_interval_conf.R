#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @param data: An object of type vector. Refers to the data where the range will be calculated
#' @param conf_level: Must be a numeric value between 0 and 1. Refers to the confidence level of the calculated interval
#' @param type: Must be: 'two_sided', 'bottom' or 'upper'. Refers to whether the calculated range should be two-sided or one-sided

#' @export
boot_interval_conf=function(data,conf_level,type='two_sided'){
  #defining confiability
  if(type=='two_sided')
  {
    alpha=(1-conf_level)/2
    q1=quantile(data,probs=alpha,names = F)
    q3=quantile(data,probs=1-alpha,names = F)
    df=data.frame(Confidence=conf_level,Bottom_lim=q1,Upper_lim=q3)
  }
  else
    if(type=="bottom")
    {
      alpha=(1-conf_level)
      q1=quantile(data,probs=alpha,names = F)
      df=data.frame(Confidence=conf_level,Bottom_lim=q1)
    }
  else
  {
    q3=quantile(data,probs=conf_level,names = F)
    df=data.frame(Confidence=conf_level,Upper_lim=q3)
  }
  return(df)
}
