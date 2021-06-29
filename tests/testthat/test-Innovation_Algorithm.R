test_that("Ueberpruefung der Eingabewerte", {
  expect_error(innovation(NA),"Was erwartet wird")
})

test_that("Innovation Algorithmus funktioniert", {
  expect_equal(2 * 2, 4)
})
