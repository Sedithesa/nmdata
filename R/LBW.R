
#' Title
#'
#' @param sex
#' @param weight
#' @param val.BMI
#' @param len
#'
#' @return
#' @export
#'
#' @examples
#'
#'

LBW <- function(sex, weight, val.BMI=NULL, len=NULL){

  if(class(sex)=="character"){
    if(sum(!grepl("^[m|f].*$", sex))>0){stop("Column \"Sex\" contains strange values!")
    } else {
      sex <- ifelse(grepl("^m.*$", sex), 1, 0) # 1 is male, 0 is female
    }
  }


  if(is.null(len)){

    if(is.null(val.BMI)){stop("Or column \"BMI\" or column \"length\" has to be supplied.")
    } else {

      #Source: https://clincalc.com/Kinetics/IdealBW.aspx
      val.LBW <- ifelse(as.numeric(sex)==0,
                        (9.27 * 10^3 * as.numeric(weight)) / (6.68 * 10^3 + (216 * val.BMI)),
                        (9.27 * 10^3 * as.numeric(weight)) / (8.78 * 10^3 + (244 * val.BMI)))

    }
  } else {

    #Source: https://clincalc.com/Kinetics/IdealBW.aspx
    val.LBW <- ifelse(as.numeric(sex)==0,
                      (9.27 * 10^3 * as.numeric(weight)) / (6.68 * 10^3 + (216 * BMI(weight, len))),
                      (9.27 * 10^3 * as.numeric(weight)) / (8.78 * 10^3 + (244 * BMI(weight, len))))
  }

  return(val.LBW)
}
