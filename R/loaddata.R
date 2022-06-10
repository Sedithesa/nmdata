

load.data <- function(name,file_type){
  if(tolower(file_type) == 'xlsx'){
    data <- paste0(name,'.', file_type)
    db <- read_excel(data)
  }else if(tolower(file_type) == 'csv'){
    data <- paste0(name,'.','csv')
    db  <- read.csv(data)
  }else{
    db <- 'Unknown file type'
  }
  return(db)
}