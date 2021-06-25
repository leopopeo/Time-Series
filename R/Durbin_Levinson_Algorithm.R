DLA <- function(x, len = NULL) {
  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch." = is.numeric(x))
  stopifnot("Die Länge des Vektors muss größer als 1 sein." = length(x) > 1)


  stopifnot("len muss NULL oder ein Integer sein." = (is.null(len) |
                                                        is.numeric(len)))
  if (is.null(len))
    len <- n - 1
  stopifnot("len muss >= 2 sein" =  len >= 2)
  stopifnot("len muss NULL oder ein Integer Wert sein." = length(len) == 1)
  stopifnot("len muss NULL oder ein Integer Wert sein." = len %% 1 == 0)

  #Berechnung
  #Start Values
  acf_x <- ACF(x)
  Phi_nn <- acf_x[2] / acf_x[1] #per Definition
  Phi <- Phi_nn
  v <- acf_x[1] * (1 - Phi_nn ^ 2) #per Definition

  #Rekursion
  for (i in 2:len) {
    Phi_nn <- (acf_x[i + 1] - sum(Phi * acf_x[i:2])) * v ^ -1
    Phi <- c(Phi - Phi_nn * Phi[(i - 1):1], Phi_nn)
    v <- v * (1 - Phi_nn ^ 2)
  }
  rev(Phi)
}

########Test
#PacfDL(X)
DLA(X, len=3)
#levinson(X,p=3)

