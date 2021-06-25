
set.seed(1)

source("/Users/niklasmerz/Documents/GitHub/Time-Series/R/ACF1.R")

DLA <- function(X) {
  end <- length(X)
  L <- matrix(0, ncol = end, nrow = end)
  L[1, 1] <-
    ACF(X, lag = 1)[1] / ACF(X, lag = 0)  # Niklas: ACF(X, lag = 1)[1] hab das [1] mal dazugemacht weil sonst 2 Werte durch 1 Wert geteilt werden. hab mir die Formel aber nicht angeschaut, kann also auch falsch sein
  v_n <- ACF(X, lag = 0)
  for (i in 2:length(X)) {
    z <- NULL
    v <- v_n * (1 - L[i, i])
    for (j in 1:(end - 1) ){
      z <- L[end - 1, j] * ACF(X, lag = end - j)
    }
    z <- sum(z)
    L[i, i] <- (ACF(X, lag= i)[i] - z) * (1 / (v))
  }
  v_n <<- v
  return(L)
}

DLA(X)

levinson(X,p=length(X))

levinson

"
gen_AR <- function(start, phi, sd, I, single = F){
  p = length(phi)
  X = start
  if (single) eps = rnorm(I-p, sd = sd)
  if (!single) eps = rep(0, I-p)
  for (i in 1:(I-p)){
    X <- c(X, reg(X, phi)+eps)
  }
  X
}

gen_AR(start, phi, 100)

gen_MA <- function(theta, sd, I){
  q <- length(theta)
  W = rnorm(I, sd = sd )
  Z = W[1:q]
  for (i in 1:I){
    Z <- c(Z, reg(Z, theta)+rnorm(n = 1, sd=sd))
  }
  Z[(p+1):length(Z)]
}

gen_MA(theta, 1, 100)
"

gen2 <- function(start, phi = 0, theta = 0, sd, I){
  q <- length(theta)
  p = length(phi)
  X = start
  #W = rnorm(I, sd = sd )
  Z = rnorm(q, sd=sd)
  for (i in 1:I){
    Z <- c(Z, reg(Z, theta)+rnorm(n = 1, sd=sd))
    X <- c(X, reg(X, phi) + Z[i+q])
  }
  X[(length(start)+1):length(X)]
}

Y1 = gen2(start, phi, theta, sd, I)
plot(Y1, type ="l")

Y2 = arima.sim(n = I,
               list(ar = phi,
                    ma = theta,
                    sd = sd))
plot(Y2, type = "l")

arima(Y1, order = c(length(phi), 0, length(theta)))
arima(Y2, order = c(length(phi), 0, length(theta)))
c(phi, theta)


gen <- function(start, phi = 0, theta = 0, sd, I){
  if (identical(phi, 0)){
    return(gen_MA(theta, sd, I))
  }
  if (identical(theta, 0)){
    return(gen_MA())
  }
}

T1 = gen2(start, sd, phi, theta, I)
plot(T1, type = "l" )
