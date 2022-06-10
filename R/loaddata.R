require(readxl)

load_ <- function(name,format){
  if(tolower(format) == 'csv'){
    x = 'csv'
  }
  else if(tolower(format)== 'xlsx'){
    x = 'xlsx'
    database <- paste0(name,'.',format)
    print(database)
    db <- read_excel(database, na = ".")
  }else{
    x = 'Unknown file format'
  }
  return(db)
}

