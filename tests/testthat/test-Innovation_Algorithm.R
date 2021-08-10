test_that("Test der Eingabewerte", {
  #Zeitreihe test
  expect_error(innovation(c("Test")), "Eingabe ist nicht numerisch!")
  expect_error(innovation(NA), "Eingabe ist nicht numerisch!")
  expect_error(innovation(list(1,2,3)), "Eingabe ist nicht numerisch!")
  expect_error(innovation(c(1)), "Die Laenge des Vektors muss groeßer als 1 sein!")

  #lag test
  expect_error(innovation(c(1,2,3,4), lag = 1.1), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(innovation(c(1,2,3,4), lag = c(1,1)), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(innovation(c(1,2,3,4), lag ="Test"), "lag muss NULL oder ein Integer Wert sein!")
})

test_that("Innovation Algorithmus funktioniert", {
  #Wir testen von Hand für n = 4
  X = c(0.5, 1, -1/3, 1/4, 0 )
  COV = ACF(X) #Wird an anderer Stelle getestet
  #n = 1
  v0 = COV[1]
  t11 = COV[2]/v0
  v <- v0-t11*t11*v0
  #n = 2
  t22 <- COV[3]/v0
  t21 <- (COV[2]-t11*t22*v0)/v
  v[2] <- v0-t22^2*v0-t21^2*v
  #n = 3
  t33 <- COV[4]/v0
  t32 <- 1/v[1]*(COV[3]-t33*t11*v0)
  t31 <- 1/v[2]*(COV[2]-t33*t22*v0-t21*t32*v[1])
  v[3] <- v0 - t33^2*v0-t32^2*v[1]-t31^2*v[2]
  #n = 4
  t44 <- COV[5]/v0
  t43 <- 1/v[1]*(COV[4]-t44*t11*v0)
  t42 <- 1/v[2]*(COV[3]-t44*t22*v0-t43*t21*v[1])
  t41 <- 1/v[3]*(COV[2]-t44*t33*v0-t43*t32*v[1]-t42*t31*v[2])
  v[4] <- v0 - t44^2*v0-t43^2*v[1]-t42^2*v[2]-t41^2*v[3]

  #Testing
  theta <- matrix(c(t11,t21,t31,t41,0,t22,t32,t42,0,0,t33,t43,0,0,0,t44), nrow = 4)
  expect_equal(innovation(ts = X, lag = 4), theta)
})
## Niklas: Mit was kann ich den Algorithmus testen? Gibt es eine R-Funktion die das gleiche ausrechnet?
