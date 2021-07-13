test_that("Eingabe testen", {
  expect_error(perio(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(perio(NA), "Eingabe ist nicht numerisch!")
  expect_error(perio(c(1)), "Die Länge des Vektors muss größer als 1 sein!")
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
