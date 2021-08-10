#'Periodogramm
#'
#'\code{perio} bestimmt die spektrale Dichte des Signals. Dadurch koennen die dominanten Frequenzen einer Zeitreihe identifiziert werden. Fuer mehr Informationen rufe die Vignette auf.
#'
#'@param y Eingabevektor, der die beobachteten Daten enthaelt.
#'@return Vektor, der die Periodogrammwerte bzw. die Zeitreihe bei den Fourierfrequenzen enthaelt.
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(phi=0.3,sd=1,I=1000)
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
    Res[1+w+ceiling(n/2)] <- (1/n)*abs(sum)^2 #der Faktor:1+w+ceiling(n/2) wird gebraucht um den Vektor Res nacheinander ab dem Index 1 aufzufuellen
  }
  Res
}


