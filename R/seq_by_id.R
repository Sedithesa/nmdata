#' Create sequences for NONMEM dataset
#'
#' @param id_col Values from the (patient) ID column
#' @param occ_col Values from the occasion (OCC) column
#' @export
#' @examples  nmdataset$SEQ_IDS <- seq_per_id(ds$ID)
#' @examples  nmdataset$SEQ_OCC <- seq_per_occ(ds$ID, ds$OCC)


seq_per_id <- function(id_col){
  ave(1:length(id_col), id_col, FUN=function(x){1:length(x)})
}

