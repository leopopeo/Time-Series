test_that("Eingabe testen", {
  expect_error(ts_predict(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(ts_predict(NA), "Eingabe ist nicht numerisch!")
  expect_error(ts_predict(NULL), "Eingabe ist nicht numerisch!")
  expect_error(ts_predict(c(1)), "Die Länge des Vektors muss größer als 1 sei!")

  expect_error(ts_predict(c(3,4,5,1), steps=1.1), "len muss >= 2 sein!")
  expect_error(ts_predict(c(3,4,5,1), steps=c(1,1)), "len muss >= 2 sein!")
  expect_error(ts_predict(c(3,4,5,1), steps=NA), "len muss NULL oder ein Integer Wert sein!")
  expect_error(ts_predict(c(3,4,5,1), steps="a"), "len muss NULL oder ein Integer Wert sein!")

  expect_error(ts_predict(c(3,4,5,1), steps=2, all= c("a","b"),"Eingabe ist kein logical Wert"))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= 1.1,"Eingabe ist kein logical Wert"))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all=NA,"Eingabe ist kein logical Wert"))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= "a","Eingabe ist kein logical Wert"))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= c(1,2),"Eingabe ist kein logical Wert"))
})

