
#' Create table one
#'
#' @param db Database containing patient characteristics
#' @param id Name of ID column
#'
#' @import tidyverse
#' @export
#'


table1 <- function(db, id='ID'){
  colnames(db)[colnames(db) == id] <- 'ID'
  table <- db %>% group_by(ID) %>% mutate(median_amt = median(AMT, na.rm=T),
                                          median_DV = median(DV, na.rm=T)) %>%
    distinct(ID, .keep_all = T)

  return(table)
}
