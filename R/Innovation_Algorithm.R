#'Innovation Algorithm
#'
#'@description  Beschreibung
#'Der Innovations Algorithmus bestimmt .
#'
#'
#'Details
#'
#'@param theta
#'@param v
#'@param n
#'@param k
#'
#'@return Numerischer Vektor, der die Vorhersage des Innovation Algorithmus beinhaltet.
#'
#'@examples innovation(arima.sim(n = I, list(ar = c(0.8897,-0.4858),ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796)), 10)
#'
#'@export
#Implementierung des Innovations Algorithmus
#innovations_algorithm <- function(timeseries,){
  #Eingabe ueberpruefen
  #Timeseries timeseries ueberpruefen



  #Rekursive Berechnung der Koeffizienten Theta_n_1,..., Theta_n_n

  #Berechnung des mittleren quadratischen Abweichung des ersten Elements


#Hilfsfunktion
theta_sum <- function(theta, v, n, k){
  if (k == 0) return(0)
  x = NULL
  for (j in 0:(k-1)){
    x <- c(x, theta[k,k-j]*theta[n,n-j]*v[j+1])
  }
  return(sum(x))
}


#Berechnung des Thetas
innovation <- function(ts,
                       small_theta = matrix(0, lag, lag),
                       lag = NA){
  theta <- matrix(0, lag, lag)
  theta[1:nrow(small_theta), 1:ncol(small_theta)] <- small_theta
  n_start <- which(theta[,1] == 0)[1]
  COV = ACF(ts, lag = lag)
  v = COV[1]
  for (n in (1:n_start-1)){
    v[n+1] <- COV[1] - theta_sum(theta, v, n, n)
  }
  for (n in n_start:lag){
    for (k in 0:(n-1)){
      theta[n,n-k] <- 1/v[k+1]*(COV[n-k+1] - theta_sum(theta, v, n, k))
    }
    v[n+1] <- COV[1] - theta_sum(theta, v, n, n)
  }
  return(theta)
}



########################Test


X = arima.sim(n = 500, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))





R = innovation(X, lag = 200)

start_time1 <- Sys.time()
S1 = innovation(X, lag =  400)
end_time1 <- Sys.time()

start_time2 <- Sys.time()
S2 = innovation(X, R, lag = 400)
end_time2 <- Sys.time()


print(end_time1 - start_time1)
print(end_time2 - start_time2)
