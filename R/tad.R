
#' Calculate time after dose
#'
#' @param DATE Date
#' @param TIME Time
#' @param method method to calculate TAD
#' @keywords TAD
#' @export
#' @examples -


# Renames date and time columns as DATE and TIME.
# Takes character values of DATE and TIME columns in format specified
# in the function. Then creates a date time (DTTM) column.

tad <- function(database,date='DATE',time='TIME',dose='DOSE',id='ID'){
  TAD <- c()
  db = database
  colnames(db)[colnames(db) == date] <- 'DATE'
  colnames(db)[colnames(db) == time] <- 'TIME'
  colnames(db)[colnames(db) == dose] <- 'DOSE'

  db$DTTM <- as.POSIXct(paste(db$DATE, db$TIME), format="%m/%d/%Y %H:%M:%S")

  # arrange per ID and DTTM
  db <- db[order(db$ID,db$DTTM),]

  ldose <- 0

  # Without ADDL
  if(!('ADDL' %in% colnames(db))){
    for(i in 1:nrow(db)){
      if(!is.na(db$DOSE[i])){
        ldose = db$DTTM[i]
        TAD <- c(TAD,0)
      }else{
        TAD <- c(TAD, difftime(db$DTTM[i],ldose, units = 'hours'))
      }
    }
  }

  db2 <- cbind(db,TAD)
  db2$TAD <- as.numeric(db2$TAD)
  return(db2)
}


