#'ARMA Generator
#'
#'@description Diese Funktion simuliert den ARMA-Prozess.
#'
#'Das ARMA-Model beschreibt einen stochatischen Prozess mittels zwei Polynomen:
#'Der Autoregression (AR) und dem "moving average" (MA).
#'
#'@param start
#'@param phi
#'@param theta
#'@param sd
#'@param I
#'
#'@return
#'@examples
#'gen()
#'gen()
#'
#'@export

#Parameter setzen für die Beispiele unten. NICHT TEIL DER ENTGÜLTIGEN VERSION.
set.seed(1)
start = runif(3)
phi = c(0.8, -0.3, 0.1)
theta = c(0.8, -0.3, 0.3)
sd = 0.2
I = 100

<<<<<<< Updated upstream
#Funktion zur Berechnung der Regression
=======
#'Regression auf letzte Werte
#'
#'\code{reg} berechnet den nächsten Wert einer Zeitreihe als gewichtete Summe der Vorangegangenen Werte
>>>>>>> Stashed changes
reg = function(X, coeff){
  n <- length(coeff)
  sum(coeff*X[length(X):(length(X)-(n-1))])
}

#Generator-Funktion
gen <- function(start, phi = 0, theta = 0, sd, I){
  #Hier ueberpruefung wir die Eingabewerte auf ihre Richtigkeit
  # start
 # stopifnot("" = )

  # phi
  stopifnot("Phi muss ein numerischer Vektor oder NULL sein." = is.numeric(phi) | is.null(phi))

  # theta
  stopifnot("Theta muss ein numerischer Vektor oder NULL sein." = is.numeric(theta) | is.null(theta))

  # sd
  stopifnot("Die Standardabweichung sd muss ein numerischer Vektor der Länge 1 sein." = length(sd) == 1 & is.numeric(sd))

  # I
#  stopifnot("" = )


  q <- length(theta)
  p = length(phi)
  X = start
  Z = rnorm(q, sd=sd)
  for (i in 1:I){
    Z <- c(Z, rnorm(n = 1, sd=sd))
    X <- c(X, reg(X, phi) + reg(Z[-length(Z)], theta) + Z[i+q])
  }
  X[(length(start)+1):length(X)]
}

#start evtl nicht als Argument sondern als Zufallsvektor in Fkt erstellen
#
Y1 = gen(start, phi, theta, sd, I)
plot(Y1, type ="l")

Y2 = arima.sim(n = I,
               list(ar = phi,
                    ma = theta,
                    sd = sd))
#plot(Y2, type = "l")

<<<<<<< Updated upstream
#Von Leo: Hier das habe ich ausgeklammert. Bei mir kam ne Fehlermeldung deswegen.
'arima(Y1, order = c(length(phi), 0, length(theta)))
arima(Y2, order = c(length(phi), 0, length(theta)))
c(phi, theta)'
=======
arima(Y1, order = c(length(phi), 0, length(theta)),
      optim.control = list(maxit = 1000))
arima(Y2, order = c(length(phi), 0, length(theta)),
      optim.control = list(maxit = 1000))
c(phi, theta)
>>>>>>> Stashed changes


