#'ts_predict
#'
#'@description
#'N-Schritt Prognose mithilfe des Innovation Algorithmus.
#'
#'Details
#'
#'@param X Die Zeitreihe
#'@param steps Anzahl N der Schritte der Prognose
#'
#'@return Numerischer Vektor der Vorhersage
#'
#'@examples X = arima.sim(n = I, list(ar = c(0.8897,-0.4858),ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796)), 10)
#'ts_predict(X, 4)
#'
#'@export
#Implementierung des Innovations Algorithmus
#innovations_algorithm <- function(timeseries,){
#Eingabe ueberpruefen
#Timeseries timeseries ueberpruefen




#Rekursive Berechnung der Koeffizienten Theta_n_1,..., Theta_n_n

#Berechnung des mittleren quadratischen Abweichung des ersten Elements


#Hilfsfunktion

regress <- function(X, X_hat, theta, n){
  X = X[1:n]
  coeff <- theta[n,n:1]
  sum(coeff*(X-X_hat))
}

#Optimiert, deutlich schneller!
estimate <- function(X, X_hat = 0, n){
  if (X_hat[1] != 0) X_hat[1] <- 0
  theta = matrix(0, length(X_hat), length(X_hat))
  for (k in length(X_hat):n){
    theta = innovation(X, theta, k)
    X_hat <- c(X_hat, regress(X, X_hat, theta, k) )
  }
  X_hat
}


ts_predict <- function(X, steps){
  X_cache <- X
  X_hat <- 0
  for (i in 1:steps){
    n <- length(X_cache)
    X_hat <- estimate(X_cache, X_hat, n)
    X_cache <- c(X_cache, X_hat[n+1])
  }
  X_hat
}


set.seed(1)
X = arima.sim(n = 200, list(
  ar = c(0.95),
  ma = c(0.7, 0.25)),
  sd = sqrt(0.1796))
start_time1 <- Sys.time()
R = ts_predict(X, 4)
end_time1 <- Sys.time()

print(end_time1-start_time1)
# plot(X)
# X_hat = estimate(X, 50)
# plot(X)
# lines(X_hat, col = "red")
#X = sin(1:80)

#Niklas: Bitte solche Sachen immer in Klammer setzten weil der Path nicht bei allen funktioniert


# X_test <- X[1:60]
# X_est = ts_predict(X_test, 50)
# plot(X_est[60:length(X_est)], type = "l", col = "red")
# lines(X)



