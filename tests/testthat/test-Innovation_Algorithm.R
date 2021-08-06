test_that("Test der Eingabewerte", {
  #Zeitreihe test
  expect_error(innovation(c("Test")), "Eingabe ist nicht numerisch!")
  expect_error(innovation(NA), "Eingabe ist nicht numerisch!")
  expect_error(innovation(list(1,2,3)), "Eingabe ist nicht numerisch!")
  expect_error(innovation(c(1)), "Die Länge des Vektors muss größer als 1 sein!")

  #lag test
  expect_error(innovation(c(1,2,3,4), lag = 1.1), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(innovation(c(1,2,3,4), lag = c(1,1)), "lag muss NULL oder ein Integer Wert sein!")
  expect_error(innovation(c(1,2,3,4), lag ="Test"), "lag muss NULL oder ein Integer Wert sein!")
})

test_that("Innovation Algorithmus funktioniert", {
  expect_equal(2 * 2, 4)
})
## Niklas: Mit was kann ich den Algorithmus testen? Gibt es eine R-Funktion die das gleiche ausrechnet?
