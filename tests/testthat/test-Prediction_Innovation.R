test_that("Eingabe testen", {
  expect_error(ts_predict(c("a","b")), "X muss ein numerischer Vektor min. der Laenge 2 sein.")
  expect_error(ts_predict(NA), "X muss ein numerischer Vektor min. der Laenge 2 sein.")
  expect_error(ts_predict(NULL), "X muss ein numerischer Vektor min. der Laenge 2 sein.")
  expect_error(ts_predict(c(1)), "X muss ein numerischer Vektor min. der Laenge 2 sein.")

  expect_error(ts_predict(c(3,4,5,1), steps=1.1), "steps muss Integer Vektor der Laenge 1 sein.")
  expect_error(ts_predict(c(3,4,5,1), steps=c(1,1)), "steps muss Integer Vektor der Laenge 1 sein.")
  expect_error(ts_predict(c(3,4,5,1), steps=NA), "steps muss Integer Vektor der Laenge 1 sein.")


  expect_error(ts_predict(c(3,4,5,1), steps=2, all= c("a","b"), "all muss logischer Wert sein."))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= 1.1, "all muss logischer Wert sein."))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all=NA, "all muss logischer Wert sein."))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= "a", "all muss logischer Wert sein."))
  expect_error(ts_predict(c(3,4,5,1), steps=2, all= c(1,2), "all muss logischer Wert sein."))
})

