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


theta_sum <- function(theta, v, n, k){
  if (k == 0) return(0)
  x = NULL
  for (j in 0:(k-1)){
    x <- c(x, theta[k,k-j]*theta[n,n-j]*v[j+1])
  }
  return(sum(x))
}

#Berechnung des Thetas
innovation <- function(ts, lag = NA){
  theta <- matrix(0, lag, lag)
  COV = ACF(ts, lag = lag)
  v = COV[1]
  for (n in 1:lag){
    for (k in 0:(n-1)){
      theta[n,n-k] <- 1/v[k+1]*(COV[n-k+1] - theta_sum(theta, v, n, k))
    }
    v[n+1] <- COV[1] - theta_sum(theta, v, n, n)
  }
  return(theta)
}



########################Test

#########################Vorlage
innovation2 <- function(ts,lag.max=NA)
{
  #1 Check Inputs:
  #ts Check
  if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
  if(is.numeric(ts)) x <- ts
  if(class(ts)=="arma") x <- ts$arma

  # lag.max Check
  if( all(!is.numeric(lag.max),!is.na(lag.max)) ) stop("lag.max must be integer or NA!")
  if(length(lag.max)!=1) stop("length of lag.max must equal 1")
  if(length(x)<=1) stop("length of ts must be greater than 1")
  n <- length(x)
  if( is.na(lag.max) ) lag.max <- n-1
  if( lag.max%%1!=0 ) stop("lag.max must be integer or NA!")
  if(lag.max > n-1 | lag.max <= 0) stop("lag.max must be between 1 and length(ts)-1")


  theta <- matrix(rep(0,(lag.max)*(lag.max)),nrow = lag.max)
  #acf vec
  acf_vec <- ACF(ts,lag = lag.max)
  #mean squared errors v
  v <- acf_vec[1]
  if (acf_vec[1]==0) stop("Variance cant be zero")

  thetasum <- function(theta,i,k,v)
  {
    if(k==0)
      return(0)
    theta_k <- theta[k,k:1]
    theta_i <- theta[i,i:(i-k+1)]
    v_ <- v[1:k]
    sum(v_*theta_k*theta_i)
  }
  #Thetas calculate
  for (i in seq(lag.max))
  {
    for (k in seq(0,i-1))
    {
      theta[i,i-k] <- 1/v[k+1]*(acf_vec[i-k+1] - thetasum(theta,i,k,v))
    }
    v[i+1] <- acf_vec[1]-thetasum(theta,i,i,v)
  }

  theta
}

X = arima.sim(n = 100, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))
################################

innovation2(as.double(X), 10)
innovation(X, 10)
