
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

