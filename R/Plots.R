#<<<<<<< HEAD
plot_timeseries <- function(timeseries){
  #Eingabe ueberpruefen
  stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(timeseries))
  stopifnot("Der Vektor timeseries muss wenigstens die L?nge 1 haben." = length(timeseries) < 0)


  #Timeseries plotten
  tibble2plot <- tibble::tibble(values = timeseries, time = length(timeseries))
  plt_base <- ggplot2::ggplot(data = tibble2plot)
  lay <- ggplot2::geom_point(mapping(x = time, y = values))
  plt <- plt_base + lay
  plt
}
#=======


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
#>>>>>>> 3f4bb99b701e2a5f3a4066b2226502bf63819d0a
