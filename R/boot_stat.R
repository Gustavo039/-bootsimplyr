#' Given a vector of size N, the matrix N X simu_size will be returned. Where each row is a sample observation and each column is a sample
#' @param data: data: An object of type data.frame. Refers to the data where the static function calculation will be performed
#' @param FUN: Must be a function. Refers to the statistical function to be applied on a date


#' @export
boot_stat=function(data,FUN){
  stat=apply(data,2,FUN)
  if(!is.null(stat))
    return(stat)
  else
  {
    print('The argument FUN does not match with any statistical function')
  }
}
