
#' function to import data file based on file type (either 'xlsx' or 'csv')
#'
#' @param name file name without extension
#' @param file_type Type of file xlsx or csv
#' @keywords xlsx csv
#' @export
#' @examples -



load.data <- function(name,file_type,na_value){
  if(tolower(file_type) == 'xlsx'){
    data <- paste0(name,'.', file_type)
    if(missing(na_value)){
      db <- readxl::read_excel(data)}
    else{
      db <- readxl::read_excel(data, na = na_value)
    }
  }else if(tolower(file_type) == 'csv'){
    data <- paste0(name,'.','csv')
    if(missing(na_value)){
      db  <- read.csv(data)}
    else{
      db  <- read.csv(data, na.strings = na_value)
    }
  }else{
    db <- 'Unknown file type'
  }
  return(db)
}

