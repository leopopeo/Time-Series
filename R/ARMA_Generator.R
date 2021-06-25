set.seed(1)

start = runif(3)
phi = c(0.8, -0.3, 0.1)
theta = c(0.8, -0.3, 0.3)
sd = 0.2
I = 100

reg = function(X, coeff){
  n <- length(coeff)
  sum(coeff*X[length(X):(length(X)-(n-1))])
}


gen <- function(start, phi = 0, theta = 0, sd, I){
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

arima(Y1, order = c(length(phi), 0, length(theta)))
arima(Y2, order = c(length(phi), 0, length(theta)))
c(phi, theta)


