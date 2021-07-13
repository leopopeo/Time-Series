test_that("Eingabe testen", {
  #Test des Eingabevektors
  expect_error(ACF(c("a","b")), "Eingabe ist kein numerischer Vektor!")
  expect_error(ACF(NA), "Eingabe ist kein numerischer Vektor!")
  expect_error(ACF(NULL), "Eingabe ist kein numerischer Vektor!")
  expect_error(ACF(c(3)), "Die Länge des Vektors muss größer als 1 sein!")

  #Test der lag Eingabe
  expect_error(ACF(c(3,4,5,1), lag=1.1), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(ACF(c(3,4,5,1), lag=c(1,1)), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(ACF(c(3,4,5,1), lag=NA), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(ACF(c(3,4,5,1), lag="a"), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(ACF(c(3,4,5,1), lag=10), "lag liegt nicht im richtigen Bereich. Der richtige Bereich liegt zwischen -n und n!")
})

test_that("ACF Implementation funktioniert", {
  X <-  arima.sim(n = 100, list(ar = c(0.8897,-0.4858),ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
  expect_equal(ACF(X, lag= 4), as.vector(unclass(acf(X,lag.max = 4,type = "covariance",plot = FALSE))$acf))
  expect_equal(ACF(X)[1], mean((X-mean(X))^2) )
  expect_equal(ACF(X, lag= 0), mean((X-mean(X))^2))
})
