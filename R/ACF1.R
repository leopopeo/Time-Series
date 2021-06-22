# Sample ACF
set.seed(42)


#' Dies ist die Covarianz Funktion
#' Sie berechnet die Covarianz
#' Die ist manchmal groß
#' und manchmal klein
covariance <- function(X, start = 1, end = length(X), h) {
  stopifnot("h ist nicht im richtigen Bereich" = (-end < h | h < end))
  stopifnot("Eingabe ist nicht numerisch" = is.numeric(X))
  stopifnot("Eingabe ist nicht größer als eins" = end > 0)
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

ACF <- function(X, start = 1, end = length(X), type, h){
  if (type == "covariance") covariance(X, start, end, h)
  if (type == "correlation") covariance(X, start, end, h)/covariance(X, start, end, 0)
}

############Test
I <- 1000
X = arima.sim(n = I,
              list(ar = c(0.5, 0.499),
                   ma = c(-0.2279, 0.2488)),
              sd = sqrt(0.1796))
plot(X, type = "l")

ACF(X, type = "correlation", h = 1)
ACF(X, type = "correlation", h = 2)
ACF(X, type = "correlation", h = 3)
ACF(X, type = "correlation", h = 4)

acf(X, lag.max = 4, type = "correlation", plot = FALSE)

