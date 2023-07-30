
#' Create sequences for NONMEM dataset
#' @description
#' Create sequences for NONMEM dataset
#'
#' @param id_col Values from the (patient) ID column
#' @param occ_col Values from the occasion (OCC) column
#' @export
#' @keywords seq_by_occ


seq_by_occ <- function(id_col, occ_col){
  ave(1:length(id_col), id_col, occ_col, FUN=function(x){1:length(x)})
}
