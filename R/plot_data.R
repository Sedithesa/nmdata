#' Plot raw nonmem data
#'
#' @param database name of the database
#' @param time Name of the time column you want on the x-axis (e.g. 'TAD')
#' @param dv Dependend variable (e.g. concentration 'DV')
#'
#' @examples


data_plot <- function(database, time = 'TAD', dv = 'DV'){
  ggplot(database) +
    geom_point(aes_string(time,dv))+ xlim(0,24) + theme_bw()
}

