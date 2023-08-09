#' Plot raw nonmem data
#'
#' @param database name of the database
#' @param time Name of the time column you want on the x-axis (e.g. 'TAD')
#' @param dv Dependend variable (e.g. concentration 'DV')
#' @param xmax optional: limit the x-axis to a value (only upper) (e.g. xmax=24)
#' @param logy optional: log10 transform of y scale ('yes', 'y', 'true','T')
#' @param strat optional: stratify, create facets (e.g. strat = 'CMT')
#' @export
#' @examples


plot_data <- function(database, time = 'TAD', dv = 'DV', xmax = '', logy='',
                      strat = ''){

  # Error exceptions
  if(!c(time %in% names(database))) {
    stop(paste('No column in datatset with name:', time))
  }
  if(!c(dv %in% names(database))) {
    stop(paste('No column in datatset with name:', dv))
  }

  # Database transformations
  database <- database[database$MDV==0,]

  # Options
  if(missing(xmax)){
    xmax <-  max(database$TAD)
  }

  # Plotting

  pl1 <- ggplot(database) +
    geom_point(aes_string(time,dv)) + xlim(0,xmax) +
    theme_bw()

  if(tolower(logy) == 'y' | tolower(logy) == 'yes' | tolower(logy) == 'true' |
     tolower(logy) == 't'){
    pl1 <- pl1 + scale_y_continuous(trans='log10',
                    breaks=trans_breaks('log10', function(x) 10^x),
                    labels=trans_format('log10', math_format(10^.x)))
  }

  if(!missing(strat)){
    pl1 <- pl1 + facet_wrap(strat)
  }

  return(pl1)

}

