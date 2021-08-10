#'Autokovarianz-Funktion Schaetzer.
#'
#' \code{ACF} berechnet eine Naeherung an die Autokovarianz-Funktion der Datenreihe X = x_1,...,x_n. Fuer mehr Informationen rufe die Vignette auf.
#'
#'@param x Eingabevektor der die beobachteten Daten enthaelt.
#'@param lag Zeitdifferenz der Datenreihe, welche die Funktion beruecksichtigen soll. Der Default-Wert ist die komplette Zeitreihe mit einem Wert weniger als die Anzahl der Beobachtungen der Zeitreihe.
#'
#'@return Der Rueckgabewert ist ein Vektor fuer die Abschaetzung der Autokorrelationsfunktion.
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(phi=0.3,sd=1,I=1000)
#'
#'  ACF(X, lag = 4)
#'
#'@export

ACF <- function(x, lag = NULL) {
  n <- length(x)

  #Eingabewerte überpruefen
  stopifnot("Eingabe ist kein numerischer Vektor!" = is.numeric(x))
  stopifnot("Die Laenge des Vektors muss groeßer als 1 sein!" = n > 1)

  stopifnot("lag muss NULL oder ein Integer Wert sein!" = (is.null(lag) |
                                                             is.numeric(lag)))
  if (is.null(lag))
    lag <- n - 1
  stopifnot("lag muss NULL oder ein Integer Wert sein!" = length(lag) ==
              1)
  stopifnot("lag muss NULL oder ein Integer Wert sein!" = lag %% 1 == 0)
  stopifnot(
    "lag liegt nicht im richtigen Bereich. Der richtige Bereich liegt zwischen -n und n!" = (-n <= lag &
                                                                                               lag <= n)
  )
  # Definitionsbereich von lag abweichend von Brockwell aber notwendig, damit innovation funktioniert.

  #Berechnung

  # Sample mean
  x_mean <- mean(x)
  # if lag=0 -> return variance
  if (lag == 0) {
    return(mean((x - x_mean) ^ 2))
  }

  #Formel
  zwischen_mat <-
    cbind(x, sapply(1:lag, function(k)
      c(x[-(1:k)], rep(NA, times = k))))

  res <- apply(zwischen_mat, 2, function(x_lag) {
    1 / n * sum((x_lag[!is.na(x_lag)] - x_mean) * (x[!is.na(x_lag)] - x_mean))
  })
  unname(res)
}
