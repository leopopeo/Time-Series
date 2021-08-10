#Implementierung des Innovation Algorithmus

#Hilfsfunktion
theta_sum <- function(theta, v, n, k){
  if (k == 0) return(0)
  x = NULL
  for (j in 0:(k-1)){
    x <- c(x, theta[k,k-j]*theta[n,n-j]*v[j+1])
  }
  return(sum(x))
}

#'Innovation Algorithm
#'
#'@description Der Innovation ist ein Vorhersage-Algorithmus, sagt fuer eine Zeitreihe also weitere Werte vorher.
#'
#'Details
#'
#'@param ts Die Zeitreihe, welche uebergeben wird.
#'@param small_theta Optional eine Innovation-Matrix aus einer vorherigen Berechnung.
#'@param lag Der Index n bis zu welchem die Matrix berechnet werden soll.
#'
#'@return n x n Matrix mit den Innovation-Koeffizienten theta.
#'
#'@examples innovation(arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100))
#'@export
innovation <- function(ts,
                       small_theta = matrix(0, lag, lag),
                       lag = 1){
  #Eingabewerte testen
  laen <- length(ts)

  #Eingabewerte ueberpruefen
  #ts
  stopifnot("Eingabe ist nicht numerisch!" = is.numeric(ts))
  stopifnot("Die Laenge des Vektors muss groeßer als 1 sein!" = length(ts) > 1)

  #lag
  stopifnot("lag muss >= 1 sein!" =  lag >= 1)
  stopifnot("lag muss NULL oder ein Integer Wert sein!"  = (is.null(lag) |
                                                              is.numeric(lag)))
  stopifnot("lag muss NULL oder ein Integer Wert sein!" = length(lag) == 1)
  stopifnot("lag muss NULL oder ein Integer Wert sein!" = lag %% 1 == 0)

  #Berechnung
  theta <- matrix(0, lag, lag)
  theta[1:NROW(small_theta), 1:NCOL(small_theta)] <- small_theta
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
