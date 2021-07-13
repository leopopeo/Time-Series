#'Periodogram
#'
#'\code{perio} bestimmt die spektrale Dichte des Signals. Dadurch kann die dominaten Frequenzen einer Zeitreihe identifiziert werden. Für mehr Informationen rufe die Vignette auf.
#'
#'@param y Eingabevektor der die beobachteten Daten enthält
#'@return Vektor der die Periodogramwerte bzw. die Zeitreihe bei den Fourierfrequenzen enthält.
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arima.sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
#'
#'  perio(X)
#'@export

perio <- function(y){
  n <- length(y)

  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch!" = is.numeric(y))
  stopifnot("Die Länge des Vektors muss größer als 1 sein!" = n > 1)

  #Berechnung
  Res <- double(n)
  Range <- floor((-(n-1)/2):(n/2))
  for(w in Range){
    sum <- 0
    for(i in 1:n){
      sum <- sum + y[i]*exp(as.complex(-1i)*i*2*pi*w/n)
    }
    Res[w+ceiling(n/2)] <- (1/n)*abs(sum)^2
  }
  Res
}


#Nachher löschen
# #####Testing
#library(TSA)
I <- 100
X = arima.sim(n = I, list(
   ar = c(0.8897,-0.4858),
   ma = c(-0.2279, 0.2488)
 ), sd = sqrt(0.1796))

a <- periodogram(X,plot=F)
a
perio(X)

