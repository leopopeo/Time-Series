#'Time Series Plot
#'
#'@description Diese Funktion plottet eine gegebene Timeseries (deutsch: Zeitreihe).
#'
#'@param timeseries Ein numerischer Vektor als Zeitreihe.
#'@param pred Optionaler Vektor einer Vorhersage der Zeitreihe
#'@param title  Optional Character as Title
#'@return Plot der Zeitreihe.
#'@examples
#' #Erstelle eine Zeitreihe
#' X = arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100)
#' f = ts_predict(X, 10)
#' plot_timeseries(X, f, title = "Vorhersage")
#'@export

#Neu von Niklas, oberer hat bei mir nicht funktioniert??
plot_timeseries <- function(timeseries, pred = NULL, title = "Zeitreihe mit Vorhersage"){
  #Eingabe ueberpruefen
  stopifnot("Der Eingabevektor timeseries ist nicht numerisch." = is.numeric(timeseries))
  stopifnot("Der Vektor timeseries muss wenigstens die Laenge 1 haben." = length(timeseries) > 0)
  stopifnot("Der Eingabevektor pred ist nicht numerisch oder NULL." = (is.numeric(pred) | is.null(pred)))
  stopifnot("title muss ein character-Vektor der Länge 1 sein" = is.character(title) & length(title) == 1)
  if (is.null(pred)){
    #Timeseries plotten
    tibble2plot <- tibble::tibble(Wert = timeseries, Zeit = seq_along(timeseries))
    plt_base <- ggplot2::ggplot(data = tibble2plot,mapping = ggplot2::aes(x = Zeit, y = Wert))
    lay <- ggplot2::geom_line(color="#6a93b0")
    point <- ggplot2::geom_point(color="black")
    labs <- ggplot2::ggtitle("Zeitreihe")
    title = ggplot2::ggtitle(title)
    theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=15))
    plt <- plt_base + lay + labs + point + title + theme
    plt
  }
  else{
    #Timeseries mit Vorhersage plotten
    value_tbl <- tibble::tibble(Wert = timeseries, Zeit = seq_along(timeseries))
    value_tbl$Group <- "Series"

    pred_tbl <- tibble::tibble(Wert = pred, Zeit = length(timeseries)+seq_along(pred))
    pred_tbl$Group <- "Forecast"

    tibble2plot <- rbind(value_tbl, pred_tbl)


    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Zeit, y = Wert))
    lay1 <- ggplot2::geom_line(color = "#F8766D")
    lay2 <- ggplot2::geom_line(ggplot2::aes(colour = factor(Group)))
    point <- ggplot2::geom_point(data=tibble2plot, ggplot2::aes(x=Zeit,y=Wert))
    labs <- ggplot2::labs(colour = "")
    title <- ggplot2::ggtitle(title)
    theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=15))
    plt <- plt_base + lay1 + lay2 + title + labs + point + theme
    plt
  }
}

#'Periodogramm Plot
#'
#'@description Diese Funktion plottet eine gegebenes Periodogram.
#'
#'@param periodogram Ein numerischer Vektor welcher ein Periodogram einer Zeitreihe enthaelt
#'@param logscale Wenn TRUE wird die Y-Achse logarithmisch skaliert.
#'@return Plot des Periodograms in Abhaengigkeit der Fourier Frequenzen
#'@examples
#' #Erstelle ein Periodogram einer Zeitreihe
#' X = perio(arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100))
#' plot_periodogram(X)
#'@export
plot_periodogram <- function(periodogram, logscale = F){
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
  if (logscale) trans <- ggplot2::scale_y_continuous(trans = "log10")
  else trans <- NULL
  plt <- plt_base + lay + labs + point + trans+ ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=15))
  plt
}

