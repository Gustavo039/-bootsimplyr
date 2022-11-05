#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @param data An object of type vector. Refers to the sample where the resampling method will be performed
#' @param simu_size Must be a number. Refers to the number of Bootstrap samples to be generated

#' @export
boot_sim=function(data,simu_size=100){
  n=length(data)
  bootstraped=replicate(simu_size,sample(data,n,replace = T))
  return(bootstraped)
}
