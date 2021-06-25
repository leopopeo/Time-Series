
perio <- function(y){
  n <- length(y)-1

  #Eingabewerte überprüfen

  #Berechnung
  Res <- double(n)
  F_range <- -floor(((n-1)/2):(n/2))
  for(w in F_range){
    sum <- 0
    for(i in 1:n){
      sum <- sum + y[i]*exp(as.complex(-1i)*i*2*pi*w/n)
    }
  Res[w+ceiling(n/2)] <- (1/n)*abs(sum)^2
  }
  Res
}


#####Testing
#library(TSA)
I <- 100
X = arima.sim(n = I, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))

a <- periodogram(X,plot=F)$spec
a
perio(X)

