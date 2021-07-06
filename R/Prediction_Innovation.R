regress <- function(X, X_hat, theta, n){
  X = X[1:n]
  coeff <- theta[n,n:1]
  sum(coeff*(X-X_hat))
}

estimate <- function(X, n){
  X_hat <- 0
  for (k in 1:n){
    theta = innovation(X, k)
    X_hat <- c(X_hat, regress(X, X_hat, theta, k) )
  }
  X_hat
}

X = arima.sim(n = 100, list(
  ar = c(0.8897, 0.0858),
  ma = c(-0.002279, 0.002488)),
  sd = sqrt(0.1796))

X_hat = estimate(X, 50)
plot(X)
lines(X_hat, col = "red")

ts_predict <- function(X, steps){
  X_cache <- X
  for (i in 1:steps){
    n <- length(X_cache)
    X_hat <- estimate(X_cache, n)
    X_cache <- c(X_cache, X_hat[n+1])
  }
  X_hat
}
X_test <- X[1:80]
X_est = ts_predict(X_test, 10)
plot(X_est, type = "l", col = "red")
lines(X)
########################
#Determinate Theta
lag.max = 10
steps = 1
inno_result <- innovation(X,lag.max+steps-1)
theta <- inno_result


#Sum need to Prediction of previous Elements
next_x_sum <- function(theta,x,x_next,i){
  theta_ <- theta[i,i:1]
  x_cache <- x[2:(i+1)]
  sum(theta_*(x_cache-x_next))
}
x_next <- 0 # First Prediction
#Calculate previous Elements predictionssf
for (i in seq(steps+lag.max-2)){
  x_next[i+1] <- next_x_sum(theta,x,x_next,i)
}
#Calculate lag.max+steps+1 Element prediction
theta_ <- theta[lag.max+steps-1,steps:(lag.max+steps-1)]
x_ <-x[(lag.max+steps):(steps+1)]
x_next_ <- x_next[(lag.max+steps-1):steps]
x_prediction <- sum(theta_*(x_-x_next_))

x_prediction
}
