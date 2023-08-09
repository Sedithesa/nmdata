
#' Caculate the FFM
#'
#' @param wt numeric
#' @param height numeric
#' @param gender 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param age numeric
#'
#' @return
#' @export
#'
#' @examples
#'
#' @details
#' Functions from https://academic.oup.com/jac/article/73/8/2104/4995076
#'

FFM <- function(wt, height, gender, age=NULL){

  height <- as.numeric(height)
  wt <- as.numeric(wt)

  ## Functions from https://academic.oup.com/jac/article/73/8/2104/4995076


  if(is.null(age)){

    WHS_max <-  ifelse(gender %in% 1 | tolower(gender) %in% "m", 42.92, 37.99) # 1 =male
    WHS_50  <-  ifelse(gender %in% 1 | tolower(gender) %in% "m", 30.93, 35.98) # 1 =male

    val.ffm <-  WHS_max * height**2 * (wt / (WHS_50 * height**2) + wt)

  } else {

    age <- as.numeric(age)

    val.ffm <- ifelse(gender %in% 1 | tolower(gender) %in% "m",  # 1 =male
                      0.88 + ((1 - 0.88)/(1 + (age/13.4)**(-12.7))) + (9270 * wt)/(6680 + (216 * (wt/height**2))),
                      1.11 + ((1 - 1.11)/(1 + (age/7.1 )**(-1.1 ))) + (9270 * wt)/(8780 + (244 * (wt/height**2)))
    )
  }

  return(val.ffm)

}
