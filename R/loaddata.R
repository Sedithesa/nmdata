
#' function to import data file based on file type (either 'xlsx' or 'csv')
#' @description
#' function to import data file based on file type (either 'xlsx' or 'csv')
#'
#' @param name file name (can use tab to open, including file format)
#' @param na_value optional: argument to give the value that specifies NA
#' @keywords xlsx csv
#' @export
#' @examples -
#' @import readxl


loaddata <- function(name,na_value){

  if(substring(tolower(name),nchar(name)-3) == '.csv'){
    name <- substr(name,1,nchar(name)-4)
    file_type <- 'csv'
    data <- paste0(name,'.', file_type)
    if(missing(na_value)){
      db  <- read.csv(data)}
    else{
      db  <- read.csv(data, na.strings = na_value)
    }
  }else if(substring(tolower(name),nchar(name)-4) == '.xlsx'){
    name <- substr(name,1,nchar(name)-5)
    file_type <- 'xlsx'
    data <- paste0(name,'.', file_type)
    if(missing(na_value)){
      db <- readxl::read_excel(data)}
    else{
      db <- readxl::read_excel(data, na = na_value)
    }
  }else{
    db <- 'File format not recognized. Should be either \'xlsx\' or \'csv\'.
    The name should include the file format extension (e.g. \'data.csv\').'
  }


  return(db)
}

