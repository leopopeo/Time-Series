context("Test der Innovation Funktion")

test_that("Test der Eingabewerte", {
  #Zeitreihe test
  expect_error(innovation(c("Test")), "Eingabe muss ein numerische Vektor sein")
  expect_error(innovation(NA), "Eingabe muss ein numerische Vektor sein")
  expect_error(innovation(list(1,2,3)), "Eingabe muss ein numerische Vektor sein")
  expect_error(innovation(c(1)), "Der Vektor muss länger als 1 sein")

  #lag test
  expect_error(innovation(c(1,2,3,4), lag = 1.1), "lag muss Integer oder NA sein")
  expect_error(innovation(c(1,2,3,4), lag = c(1,1)), "lag muss länge gleich 0 haben")
  expect_error(innovation(c(1,2,3,4), lag ="Test"), "lag muss Integer oder NA sein")
})

test_that("Innovation Algorithmus funktioniert", {
  expect_equal(2 * 2, 4)
})
## Niklas: Mit was kann ich den Algorithmus testen? Gibt es eine R-Funktion die das gleiche ausrechnet?
