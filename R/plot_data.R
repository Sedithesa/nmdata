#' Plot raw nonmem data
#'
#' @param database name of the database
#' @param time Name of the time column you want on the x-axis (e.g. 'TAD')
#' @param dv Dependend variable (e.g. concentration 'DV')
#' @export
#' @examples


plot_data <- function(database, time = 'TAD', dv = 'DV'){

  # Error exceptions
  if(!c(time %in% names(database))) {
    stop(paste('No column in datatset with name:', time))
  }
  if(!c(dv %in% names(database))) {
    stop(paste('No column in datatset with name:', dv))
  }

  # Database transformations
  database <- database[database$MDV==0,]

  # Plotting
  ggplot(database) +
    geom_point(aes_string(time,dv))+ xlim(0,24) +
    theme_bw()
}

