set.seed(1)


I = 10000
sd = 1
phi = 0 #of length p
theta  = c(0.8897, -0.4858) #of length p

start = c(0.5,2.2,0.3) #of length p


reg = function(X, coeff){
  n <- length(coeff)
  sum(coeff*X[length(X):(length(X)-(n-1))])
}

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


