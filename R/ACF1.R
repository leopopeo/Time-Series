#'Autokovarianz-Funktion-Näherung.
#'
#' \code{ACF} berechnet eine Näherung an die Autokovarianz-Funktion der Datenreihe X = x_1,...,x_n. Für mehr Informationen rufe die Vignette auf.
#'
#'
#'
#'@param X Eingabevektor der die observierten Daten enthält
#'@param lag Zeitdifferenz der Datenreihe welche die Funktion berücksichtigen soll. Der Default Wert ist die komplette Zeitreihe mit einem Wert weniger als Observationen in der Zeitreihe
#'
#'@return Der Rückgabewert ist ein Vektor für die Abschätzung der Autokorrelationsfunktion.
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arima.sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#'
#'  ACF(X, lag = 4)
#'
#'@export

ACF <- function(x, lag = NULL) {
  n <- length(x)

  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch." = is.numeric(x))
  stopifnot("Die Länge des Vektors muss größer als 1 sein." = n > 1)
  stopifnot("h liegt nicht im richtigen Bereich. Der richtige Bereich liegt zwischen -n und n." = (-n < lag |
                                                                                                     lag < n))

  stopifnot("lag muss NULL oder ein Integer sein." = (is.null(lag) |
                                                        is.numeric(lag)))
  if (is.null(lag))
    lag <- n - 1
  stopifnot("lag muss NULL oder ein Integer Wert sein." = length(lag) ==
              1)
  stopifnot("lag muss NULL oder ein Integer Wert sein." = lag %% 1 == 0)

  #Berechnung

  # Sample mean
  x_mean <- 1 / n * sum(x)
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

############Test
I <- 100
X = arima.sim(n = I, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))

ACF(X, lag= 4)


#Base-R Implementation
acf(X,lag.max = 4,type = "covariance",plot = FALSE)







