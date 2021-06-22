# Sample ACF
set.seed(42)



covariance <- function(X, start = 1, end = length(X), h) {
  stopifnot("h ist nicht im richtigen Bereich" = (-end < h | h < end))
  stopifnot("Eingabe ist nicht numerisch" = is.numeric(X))
  stopifnot("Eingabe ist nicht groeßer als eins" = end > 0)
  stopifnot("Start- oder Endpunkt ist nicht Integer" = (start %% 1 == 0 |
                                                          end %% 1 == 0))
  n <- end - start
  sample <- X
  sample_mean <- mean(sample)
  L <- NULL
  for (t in 1:(n - abs(h))) {
    L <- c(L, (sample[t + abs(h)] - sample_mean) * (sample[t] - sample_mean))
  }
  return((1 / n) * sum(L))
}
#' Dies ist die Covarianz Funktion (Title).
#'
#' \code{ACF} berechnet die Covarianz des Eingabevektors x_1,...,x_n
#'
#'@param X Eingabevektor
#'@param start Anfangszeitpunkt der Zeitreihe
#'@param end Endzeitpunkt der Zeitreihe
#'@param type Typ der Funktion ( Korrelation oder Kovarianz)
#'@param h Tutor fragen
#'
#'@return Der Rückgabewert ist eine Abschätzung der Autokorrelationsfunktion für eine observierte Datenreihe {X_t}.
#'
#'@examples
#'I <- 1000
#'
#'X = arima.sim(n = I, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#'
#'ACF(X, lag.max = 4, type = "correlation", plot = FALSE)
#'@export
ACF <- function(X, start = 1, end = length(X), type, h){
  if (type == "covariance") return(covariance(X, start, end, h))
  if (type == "correlation") return(covariance(X, start, end, h)/covariance(X, start, end, 0))
}

############Test
I <- 1000
X = arima.sim(n = I,
              list(ar = c(0.5, 0.499),
                   ma = c(-0.2279, 0.2488)),
              sd = sqrt(0.1796))
plot(X, type = "l")

ACF(X, type = "correlation", h = 1)
ACF(X, type = "correlation", h = 2)
ACF(X, type = "correlation", h = 3)
ACF(X, type = "correlation", h = 4)

ACF(X, lag.max = 4, type = "correlation", plot = FALSE)

