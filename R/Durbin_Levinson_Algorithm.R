#'Durbin-Levinson Algorithm
#'
#'\code{DLA} berechnet durch die rekursive Durbin-Levinson Methode die Gewichtung der Phi's fuer eine uebergebene Zeitreihe. Fuer mehr Informationen rufe die Vignette auf.
#'
#'@param x Eingabevektor, der die beobachteten Daten enthaelt
#'@param len Horizont der Vorhersage
#'@return Vektor, welcher die Gewichtung der Phi's enthaelt
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(phi=0.3,sd=1,I=1000)
#'
#'  DLA(X, len = 4)
#'@export

DLA <- function(x, len = NULL) {
  n <- length(x)

  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch!" = is.numeric(x))
  stopifnot("Die Länge des Vektors muss größer als 1 sei!" = length(x) > 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!"  = (is.null(len) |
                                                        is.numeric(len)))
  if (is.null(len))
    len <- n
  stopifnot("len muss >= 2 sein!" =  len >= 2)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = length(len) == 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = len %% 1 == 0)

  #Berechnung
  #Start Values
  acf_x <- c(ACF(x),0)
  Phi_nn <- acf_x[2] / acf_x[1] #per Definition
  Phi <- Phi_nn
  v <- acf_x[1] * (1 - Phi_nn ^ 2) #per Definition

  #Rekursion
  for (i in 2:len) {
    Phi_nn <- (acf_x[i] - sum(Phi * acf_x[i:2]))/v
    Phi <- c(Phi - Phi_nn * Phi[(i - 1):1], Phi_nn)
    v <- v * (1 - Phi_nn ^ 2)
  }
  Phi
}


