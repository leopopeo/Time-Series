#library(itsmr)

#Hilfsfunktionen
regress <- function(X, X_hat, theta, n) {
  X = X[1:n]
  coeff <- theta[n, n:1]
  sum(coeff * (X - X_hat))
}

#Berechnet x_hat
estimate <- function(X,
                     theta = matrix(0, length(X_hat), length(X_hat)),
                     X_hat = 0,
                     n) {
  if (X_hat[1] != 0)
    X_hat[1] <- 0

  for (k in length(X_hat):n) {
    theta <- innovation(X,
                        small_theta =  theta,
                        lag =  k)
    X_hat <- c(X_hat, regress(X, X_hat, theta, k))
  }
  list(X_hat = X_hat, theta = theta)
}

#'ts_predict
#'
#'@description
#'N-Schritt Prognose mithilfe des Innovation Algorithmus.
#'
#'Details
#'
#'@param X Eine stationaere Zeitreihe.
#'@param steps Anzahl N der Schritte der Prognose.
#'@param all Wenn FALSE wird nur der Vorhersagevektor der Laenge steps ausgegeben. Andernfalls auch alle bis dahin berechneten X_hat
#'
#'@return Numerischer Vektor der Vorhersage. Wenn all = FALSE, werden nur die vorhergesagten N Werte zurueckgegeben, andernfalls der ganze Vektor X_hat.
#'
#'@examples X = arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100)
#'ts_predict(X, 4)
#'
#'@export

#Implementierung des Innovations Algorithmus
ts_predict <- function(X, steps=1, all = FALSE) {
  #Eingabe ueberpruefen
  #X
  stopifnot("X muss ein numerischer Vektor min. der Laenge 2 sein." = is.numeric(X) & (length(X) >= 2))
  #steps
  stopifnot("steps muss Integer Vektor der Laenge 1 sein." = steps %% 1 == 0 & (length(steps) == 1))
  stopifnot("all muss logischer Wert sein." = is.logical(all))

  X_cache <- X
  est <- list(X_hat = 0, theta = 0)
  for (i in 1:steps) {
    n <- length(X_cache)
    est <- estimate(
      X = X_cache,
      theta = est$theta,
      X_hat = est$X_hat,
      n = n
    )
    X_cache <- c(X_cache, est$X_hat[n + 1])
  }
  if (all) return(est$X_hat)
  if (!all) return(est$X_hat[-(1:length(X))])
}
