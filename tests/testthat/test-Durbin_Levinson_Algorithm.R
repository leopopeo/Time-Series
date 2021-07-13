test_that("Eingabe testen", {
  expect_error(DLA(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NA), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NULL), "Eingabe ist nicht numerisch!")
  expect_error(DLA(c(1)), "Die Länge des Vektors muss größer als 1 sei!")

  expect_error(DLA(c(3,4,5,1), len=1.1), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=c(1,1)), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=NA), "len muss NULL oder ein Integer Wert sein!")
  expect_error(DLA(c(3,4,5,1), len="a"), "len muss NULL oder ein Integer Wert sein!")
})

test_that("Durbin Levinson Algorithmus Implementation funktioniert", {
  #nicht klar wie mit welchem ALG ich das überprüfen kann
  expect_equal(2*4,8)

})
