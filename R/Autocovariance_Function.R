#' Dies ist die Kovarianz-Funktion (Title).
#'
#' \code{ACF} Berechnet die Kovarianz des Eingabevektors X = x_1,...,x_n
#'
#'@param X Eingabevektor
#'@param start Anfangszeitpunkt der Zeitreihe
#'@param end Endzeitpunkt der Zeitreihe
#'@param type Typ der Funktion (Korrelation oder Kovarianz)
#'@param h Tutor fragen
#'
#'@return Der Rückgabewert ist eine Abschätzung der Autokorrelationsfunktion für eine beobachtete Datenreihe {X_t}.
#'
#'@examples
#'  I <- 1000
#'
#'  X = arima.sim(n = I, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#'
#'  ACF(X, lag.max = 4, type = "correlation", plot = FALSE)
#'@export

# Autokovarianzfunktion (kurz: ACF)
#ACF <- function(X,
                start = 1,
                end = length(X),
                type,
                h) {
  #Eingabewerte überprüfen
  #type
 # if (type == "covariance")
  #  return(covariance(X, start, end, h))
  #if (type == "correlation")
  #  return(covariance(X, start, end, h) / covariance(X, start, end, 0))
  #else{
  #  print("Typ ist nicht definiert.")
#  }
#}

############Test
I <- 100
X = arima.sim(n = I, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))
plot(X, type = "l")

ACF(X, type = "covariance", h = 1)
ACF(X, type = "covariance", h = 2)
ACF(X, type = "covariance", h = 3)
ACF(X, type = "covariance", h = 4)

acf(X,
    lag.max = 4,
    type = "covariance",
    plot = FALSE)

ACF1 <- function(x, lag = NULL) {
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
      c(x[-c(1:k)], rep(NA, times = k))))

  res <- apply(zwischen_mat, 2, function(x_lag) {
    1 / n * sum((x_lag[!is.na(x_lag)] - x_mean) * (x[!is.na(x_lag)] - x_mean))
  })
  unname(res)
}

# Das hier ist ein Beispiel?
I <- 100
X = arima.sim(n = I, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))
plot(X, type = "l")

acf(X, type = "covariance", plot = FALSE)

ACF1(X, lag = 20)
str(a)

ACF1(X)

