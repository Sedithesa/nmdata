require(readxl)


name <- 'Test_with_mistakes_small'
file_type <- 'xlsx'


test <- function(name,file_type){
  if(tolower(file_type) == 'xlsx'){
    data <- paste0(name,'.', file_type)
    db <- read_excel(data)
  }else if(tolower(file_type) == 'csv'){
    data <- paste0(name,'.','csv')
    db  <- data
  }else{
    db <- 'Unknown file type'
  }
  return(db)
}
