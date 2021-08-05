test_that("Eingabe testen", {
  expect_error(DL_prediction(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(DL_prediction(NA), "Eingabe ist nicht numerisch!")
  expect_error(DL_prediction(NULL), "Eingabe ist nicht numerisch!")
  expect_error(DL_prediction(c(1)), "Die Länge des Vektors muss größer als 1 sei!")

  expect_error(DL_prediction(c(3,4,5,1), len=1.1), "len muss NULL oder ein Integer Wert sein!")
  expect_error(DL_prediction(c(3,4,5,1), len=c(1,1)), "len muss NULL oder ein Integer Wert sein!")
  expect_error(DL_prediction(c(3,4,5,1), len=NA), "len muss >= 1 sein!")
  expect_error(DL_prediction(c(3,4,5,1), len="a"), "len muss NULL oder ein Integer Wert sein!")

  expect_error(DL_prediction(c(3,4,5,1), len=2, all= c("a","b"),"Eingabe ist kein logical Wert"))
  expect_error(DL_prediction(c(3,4,5,1), len=2, all= 1.1,"Eingabe ist kein logical Wert"))
  expect_error(DL_prediction(c(3,4,5,1), len=2, all=NA,"Eingabe ist kein logical Wert"))
  expect_error(DL_prediction(c(3,4,5,1), len=2, all= "a","Eingabe ist kein logical Wert"))
  expect_error(DL_prediction(c(3,4,5,1), len=2, all= c(1,2),"Eingabe ist kein logical Wert"))
})


