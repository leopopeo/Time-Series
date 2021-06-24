
source("/Users/niklasmerz/Documents/GitHub/Time-Series/R/ACF1.R")


DLA <- function(X, start = 1, end = length(X), h) {
  L <- matrix(0,ncol=end,nrow=end)
  L[1,1] <- Autocovariance(1)/Autocovariance(0)
  for(i in length(x)){
    for( j in n-1)
    L[i,i] <- (Autocovariance(i)- )
  }

  stopifnot("h ist nicht im richtigen Bereich" = (-end < h | h < end))
  stopifnot("Eingabe ist nicht numerisch" = is.numeric(X))
  stopifnot("Eingabe ist nicht groeßer als eins" = end > 0)
  stopifnot("Start- oder Endpunkt ist nicht Integer" = (start %% 1 == 0 |
                                                          end %% 1 == 0))
  n <- end - start
  sample <- X
  sample_mean <- mean(sample)
  L <- NULL
  for (t in 1:(n - abs(h))) {
    L <- c(L, (sample[t + abs(h)] - sample_mean) * (sample[t] - sample_mean))
  }
  return((1 / n) * sum(L))
}



Autocovariance <- function(X, start = 1, end = length(X), type, h){
  if (type == "covariance") covariance(X, start, end, h)
  if (type == "correlation") covariance(X, start, end, h)/covariance(X, start, end, 0)
}
