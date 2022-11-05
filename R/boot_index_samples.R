#' Given a data.frame or a matrix of N rows (observations), a matrix N X simu_size will be returned. Where each row refers to the observation index in the original object and each column is a sample
#' @param data An object of type data.frame or matrix. Refers to the sample where the resampling method will be performed
#' @param simu_size Must be a number. Refers to the number of Bootstrap samples to be generated

#' @export
boot_index_samples=function(data,simu_size=100){
  n_size=nrow(data)
  index=sapply(1:simu_size,function(i){
    position=sample(1:n_size,n_size,replace = T)
    return(position)
  })
  return(index)
}
