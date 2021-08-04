#'Time Series Plot
#'
#'@description Diese Funktion plottet eine gegebene Timeseries (deutsch: Zeitreihe).
#'
#'@param timeseries Ein numerischer Vektor als Zeitreihe.
#'@return Plot der Zeitreihe.
#'@examples
#' #Erstelle eine Zeitreihe
#' X = arima.sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#' plot_timeseries(X)
#'@export

#Neu von Niklas, oberer hat bei mir nicht funktioniert??
plot_timeseries <- function(timeseries){
  #Eingabe ueberpruefen
  stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(timeseries))
  stopifnot("Der Vektor timeseries muss wenigstens die Laenge 1 haben." = length(timeseries) > 0)

  #Timeseries plotten
  tibble2plot <- tibble::tibble(Wert = timeseries, Zeit = seq_along(timeseries))
  plt_base <- ggplot2::ggplot(data = tibble2plot,mapping = ggplot2::aes(x = Zeit, y = Wert))
  lay <- ggplot2::geom_line(color="#6a93b0")
  point <- ggplot2::geom_point(color="black")
  labs <- ggplot2::ggtitle("Zeitreihe")
  plt <- plt_base + lay + labs + point + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=15))
  plt
}

#'Periodogramm Plot
#'
#'@description Diese Funktion plottet eine gegebenes PEriodogram.
#'
#'@param periodogram Ein numerischer Vektor welcher ein Periodogram einer Zeitreihe enthält
#'@return Plot des Periodograms in Abhängigkeit der Fourier Frequenzen
#'@examples
#' #Erstelle ein Periodogram einer Zeitreihe
#' X = perio(arima.sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796)))
#' plot_periodogram(X)
#'@export
plot_periodogram <- function(periodogram){
  #Eingabe ueberpruefen
  stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(periodogram))
  stopifnot("Der Vektor timeseries muss wenigstens die Laenge 1 haben." = length(periodogram) > 0)

  #Frequenzen generieren
  n <- length(periodogram)
  k <- floor((-(n-1)/2):(n/2))
  freq <- 2*pi*k/n

  #Periodogram plotten
  tibble2plot <- tibble::tibble(Wert = periodogram, Fourierfrequenz = freq)
  plt_base <- ggplot2::ggplot(data = tibble2plot,mapping = ggplot2::aes(x = Fourierfrequenz, y = Wert))
  lay <- ggplot2::geom_line(color="#6a93b0")
  point <- ggplot2::geom_point(color="black")
  labs <- ggplot2::ggtitle("Periodogramm der Zeitreihe")
  plt <- plt_base + lay + labs + point + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=15))
  plt
}

