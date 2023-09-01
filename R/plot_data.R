#' Plot raw nonmem data
#'
#' @param database name of the database
#' @param time Name of the time column you want on the x-axis (e.g. 'TAD')
#' @param dv Dependend variable (e.g. concentration 'DV')
#' @param xmax optional: limit the x-axis to a value (only upper) (e.g. xmax=24)
#' @param logy optional: log10 transform of y scale ('yes', 'y', 'true','T')
#' @param strat optional: stratify, create facets (e.g. strat = 'CMT')
#'
#' @export
#' @import ggplot2

plot_data <- function(database, time = 'TIME', dv = 'DV', xmax = '', logy='',
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
  database[[dv]] <- as.numeric(database[[dv]])



  # Options
  if(missing(xmax)){
    xmax <-  max(database[[time]], na.rm = T)
  }

  # Plotting

  pl1 <- ggplot(database) +
    geom_point(aes_string(time,dv), na.rm = T) + xlim(0,xmax) +
    theme_bw()

  if(tolower(logy) == 'y' | tolower(logy) == 'yes' | tolower(logy) == 'true' |
     tolower(logy) == 't'){
    pl1 <- pl1 + scale_y_continuous(trans='log10')
  }

  if(!missing(strat)){
    pl1 <- pl1 + facet_wrap(strat)
  }

  return(pl1)

}

