
#' function to import data file based on file type (either 'xlsx' or 'csv')
#'
#' @param name file name
#' @param file_type Type of file xlsx or csv
#' @keywords
#' @export
#' @examples



load.data <- function(name,file_type){
  if(tolower(file_type) == 'xlsx'){
    data <- paste0(name,'.', file_type)
    db <- readxl::read_excel(data)
  }else if(tolower(file_type) == 'csv'){
    data <- paste0(name,'.','csv')
    db  <- read.csv(data)
  }else{
    db <- 'Unknown file type'
  }
  return(db)
}
