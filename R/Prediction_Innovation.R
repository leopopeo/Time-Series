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
#'@param X Eine Stationäre Zeitreihe Zeitreihe
#'@param steps Anzahl N der Schritte der Prognose
#'@param all Wenn FALSe wird nur der Vorhersagevektor der L?nge steps ausgegeben. Andernfalls auch alle bis dahin berechneten X_hat
#'
#'@return Numerischer Vektor der Vorhersage. WEnn all=FALSE, werden nur die Vorhergesagten N Werte zurückgegeben, andernfalls der ganz Vektor X_hat
#'
#'@examples X = arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100)
#'ts_predict(X, 4)
#'
#'@export

#Implementierung des Innovations Algorithmus
#innovations_algorithm <- function(timeseries,){
#Eingabe ueberpruefen
#Timeseries timeseries ueberpruefen
ts_predict <- function(X, steps=1, all = FALSE) {
  #X
  stopifnot("X muss ein numerischer Vektor der min. der Länge 1sein." = is.numeric(X) & length(X) >= 1)
  #steps
  stopfifnot("steps muss Integer Vektor der Länge 1 sein" = is.integer(X) & (length(steps) == 1))
  stopfifnot("all muss logischer Wert sein" = is.logical(all))

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


##########################Alt
# ts_predict <- function(X, steps){
#   X_cache <- X
#   X_hat <- 0
#   for (i in 1:steps){
#     n <- length(X_cache)
#     X_hat <- estimate(X_cache, theta = matrix(0, length(X_hat), length(X_hat)),X_hat, n)
#     X_cache <- c(X_cache, X_hat[n+1])
#   }
#   X_hat
# }
#
#
# set.seed(1)
# X = arima.sim(n = 200, list(
#   ar = c(0.95),
#   ma = c(0.7, 0.25)),
#   sd = sqrt(0.1796))
# start_time1 <- Sys.time()
# R = ts_predict(X, 4)
# end_time1 <- Sys.time()
#
# print(end_time1-start_time1)



#
# ts_predict <- function(X, steps){
#   X_cache <- X
#   est <- list(X_hat = 0, theta = 0)
#   theta = matrix(0, length(X_hat), length(X_hat))
#   for (i in 1:steps){
#     n <- length(X_cache)
#     for (k in length(X_hat):n){
#       theta <- innovation(X, theta, k)
#       X_hat <- c(X_hat, regress(X, X_hat, theta, k) )
#     }
#     X_cache <- c(X_cache, X_hat[n+1])
#   }
#   X_hat
# }

# set.seed(3)
# X = arima.sim(n = 100, list(ar = c(0.7),
#                              ma = c(0.7, 0.25)),
#                sd = sqrt(0.02796))
# X = sin(1:50)
#
# R = ts_predict(X[1:50], 50, all = T)
#


#S = forecast(X[1:50], NULL, h = 50, arma(X, p = 10, q = 10), alpha = 1)


#lines(R, col = "green", type = "l")


