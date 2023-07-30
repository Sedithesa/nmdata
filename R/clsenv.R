#' Clearing the global environment
#'
#' @param keep.vals Vector of values that should be kept in the globalenvironment
#' @param rm If not TRUE than function returns the values that would have been removed
#'
#' @return
#' @export
#'
#' @examples
#'

clsenv <- function(keep.vals, rm=T){

  rm.vals <- ls(envir = globalenv())[!(ls(envir = globalenv()) %in% ls(envir = globalenv())[grep(paste0(paste0(c("^"),
                                                                                                               keep.vals, collapse = "|")),
                                                                                                 ls(envir = globalenv()))])]

  if(rm==T){rm(list=rm.vals, envir = globalenv())}else{return(rm.vals)}
}
