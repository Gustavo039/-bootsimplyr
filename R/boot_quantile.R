#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @params data: An object of type vector. Refers to the sample where the resampling method will be performed

#' @export
boot_quantile=function(data){
  q1=sapply(1:ncol(data),function(i){
    q=quantile(data[,i],probs=c(0.25),names=F)
    return(q)
  })
  q3=sapply(1:ncol(data),function(i){
    q=quantile(data[,i],probs=c(0.75),names=F)
    return(q)
  })
  df=data.frame(quantile_1=q1,quantile_3=q3)
  return(df)
}
