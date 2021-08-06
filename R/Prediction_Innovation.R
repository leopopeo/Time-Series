




#Rekursive Berechnung der Koeffizienten Theta_n_1,..., Theta_n_n

#Berechnung des mittleren quadratischen Abweichung des ersten Elements


#Hilfsfunktion


library(itsmr)





#Rekursive Berechnung der Koeffizienten Theta_n_1,..., Theta_n_n

#Berechnung des mittleren quadratischen Abweichung des ersten Elements

#Hilfsfunktionen

regress <- function(X, X_hat, theta, n) {
  X = X[1:n]
  coeff <- theta[n, n:1]
  sum(coeff * (X - X_hat))
}

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

####################Neu

#'ts_predict
#'
#'@description
#'N-Schritt Prognose mithilfe des Innovation Algorithmus.
#'
#'Details
#'
#'@param X Eine Stationäre Zeitreihe Zeitreihe
#'@param steps Anzahl N der Schritte der Prognose
#'@param all=FALSE
#'
#'@return Numerischer Vektor der Vorhersage. WEnn all=FALSE, werden nur die Vorhergesagten N Werte zurückgegeben, andernfalls der ganz Vektor X_hat
#'
#'@examples
#'#Erstelle eine Zeitreihe
#'X = arma_sim(theta=0.4,sd=1,I=100)
#'
#'ts_predict(X, 4)
#'
#'@export
#Implementierung des Innovations Algorithmus
#innovations_algorithm <- function(timeseries,){
#Eingabe ueberpruefen
#Timeseries timeseries ueberpruefen
ts_predict <- function(X, steps, all = FALSE) {

  laenge <- length(X)
  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch!" = is.numeric(X))
  stopifnot("Die Länge des Vektors muss größer als 1 sei!" = length(X) > 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!"  = (is.null(laenge) |
                                                              is.numeric(laenge)))

  stopifnot("len muss >= 1 sein!" =  steps >= 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = length(steps) == 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = steps %% 1 == 0)

  stopifnot("all muss ein logical-Wert sein"= is.logical(all)==T)

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

set.seed(3)
X = arima.sim(n = 100, list(ar = c(0.7),
                             ma = c(0.7, 0.25)),
               sd = sqrt(0.02796))
X = sin(1:50)

R = ts_predict(X[1:50], 50, all = T)



S = forecast(X[1:50], NULL, h = 50, arma(X, p = 10, q = 10), alpha = 1)


lines(R, col = "green", type = "l")


