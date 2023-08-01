
#' Title
#'
#' @param weight
#' @param len
#'
#' @return
#' @export
#'
#' @examples
#'

BMI <- function(weight, len){

  if(sum(len>3)>0){stop("Column \"length\" has to be supplied in meters!")}

  val.BMI <- as.numeric(weight) / (as.numeric(len)/100)**2
  return(val.BMI)

}

