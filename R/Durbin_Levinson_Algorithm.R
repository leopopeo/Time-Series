#'Durbin-Levinson Algorithm
#'
#'\code{DLA} berechnet durch die rekursive Durbin-Levinson Methode eine Vorhersage für die übergebene Zeitreihe. Für mehr Informationen rufe die Vignette auf.
#'
#'@param x Eingabevektor der die beobachteten Daten enthält
#'@param len Horizont der Vorhersage.
#'@return Vektor mit den vorhgesagten Werten
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(n = 1000, list(ar = c(0.5, 0.499), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
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
    len <- n -1
  stopifnot("len muss >= 2 sein!" =  len >= 2)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = length(len) == 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = len %% 1 == 0)

  #Berechnung
  #Start Values
  acf_x <- ACF(x)
  Phi_nn <- acf_x[2] / acf_x[1] #per Definition
  Phi <- Phi_nn
  v <- acf_x[1] * (1 - Phi_nn ^ 2) #per Definition

  #Rekursion
  for (i in 2:len) {
    Phi_nn <- (acf_x[i + 1] - sum(Phi * acf_x[i:2]))/v
    Phi <- c(Phi - Phi_nn * Phi[(i - 1):1], Phi_nn)
    v <- v * (1 - Phi_nn ^ 2)
  }
  Phi
}

DL_prediction <- function(X){

  prediction <- numeric(length(X))

  for(i in 10:length(X)){
    x_help <- X[1:i]
    dl_save <- DLA(x_help)
    prediction[i] <- sum(c(dl_save, dl_save[1])*x_help)
  }
  prediction
}

################ TEST
set.seed(1)
AR_1 <- arma_sim(phi = 0.3, sd = 1, I = 100)
DL_prediction(AR_1)

