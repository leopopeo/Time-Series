
perio <- function(y){
  n <- length(y)-1

  #Eingabewerte überprüfen

  #Berechnung
  Res <- double(n)
  range <- -unique(floor(((n-1):n)/2))
  for(w in 1:range){
    sum <- 0
    for(i in 1:n){
      Res[w] <- (1/i)*abs(sum+ y[i]*e^(as.complex(-1i)*i*(2*pi*w/n)))^2
  }}
  Res
}


#####Testing
library(TSA)
I <- 100
X = arima.sim(n = I, list(
  ar = c(0.8897,-0.4858),
  ma = c(-0.2279, 0.2488)
), sd = sqrt(0.1796))

a <- periodogram(X,plot=F)$spec
a
perio(X)

