#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @param data: An object of type data.frame. Refers to the sample where the resampling method will be performed
#' @param simu_size Must be a number. Refers to the number of Bootstrap samples to be generated
#' @param col_index1: Must be a number. Refers to the index of the date column that the correlation is to be calculated
#' @param col_index2: Must be a number. Refers to the index of the date column that the correlation is to be calculated
#' @param by: Must be: 'pearson', 'spearman' or 'kendall'. Refers to the calculated correlation method

#' @export
boot_corr_samples=function(data,simu_size=100,col_index1,col_index2,by='pearson'){
  # Data must be a data.frame type object
  n_size=nrow(data)
  correlation=sapply(1:simu_size,function(i){
    index=sample(1:n_size,n_size,replace = T)
    bootstraped=data.frame(x=data[index,col_index1],y=data[index,col_index2])
    return(cor(bootstraped$x,bootstraped$y,method = by))
  })
  return(correlation)
}
