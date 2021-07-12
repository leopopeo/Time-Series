#'Plots
#'
#'@description Diese Funktion plottet eine gegebene Timeseries (deutsch: Zeitreihe).
#'
#'@param timeseries Ein numerischer Vektor vom Typ time series.
#'@return Plot der Zeitreihe.
#'@examples
#' #Erstelle eine Zeitreihe
#' X = arima.sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#' plot_timeseries(X)
#'@export


plot_timeseries <- function(timeseries){
  #Eingabe ueberpruefen
  stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(timeseries))
  stopifnot("Der Vektor timeseries muss wenigstens die Laenge 1 haben." = length(timeseries) > 0)

  #Timeseries plotten
  tibble2plot <- tibble::tibble(values = timeseries, time = length(timeseries))
  plt_base <- ggplot2::ggplot(data = tibble2plot)
  lay <- ggplot2::geom_point(mapping(x = time, y = values))
  plt <- plt_base + lay
  plt
}



# plot_timeseries <- function(timeseries){
#   #Eingabe ueberpruefen
#   stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(timeseries))
#   stopifnot("Der Vektor timeseries muss wenigstens die L?nge 1 haben." = length(timeseries) < 0)
#
#
#   #Timeseries plotten
#   tibble2plot <- tibble::tibble(values = timeseries, time = length(timeseries))
#   plt_base <- ggplot2::ggplot(data = tibble2plot)
#   lay <- ggplot2::geom_point(mapping(x = time, y = values))
#   plt <- plt_base + lay
#   plt
# }

