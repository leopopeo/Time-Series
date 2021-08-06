test_that("Eingabe testen", {
  #Phi
  expect_error(arma_sim(phi=c("a"),sd=1,I=100), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NA), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NULL), "Eingabe ist nicht numerisch!")
  expect_error(DLA(c(1)), "Die Länge des Vektors muss größer als 1 sei!")

  #Theta
  expect_error(DLA(c(3,4,5,1), len=1.1), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=c(1,1)), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=NA), "len muss NULL oder ein Integer Wert sein!")
  expect_error(DLA(c(3,4,5,1), len="a"), "len muss NULL oder ein Integer Wert sein!")

  #sd

  #I

})



test_that("Test der Eingabewerte", {
  start = runif(3)
  sd = 0.2
  I = 100
  # Phi & Theta
  expect_error(gen(start,phi=c("b"),sd,I), "phi und theta muss ein numerischer Vektor oder NULL sein")
  expect_error(gen(start,theta=c("b"),sd,I), "phi und theta muss ein numerischer Vektor oder NULL sein!")

  # sd

  #I

})

test_that("AR works", {
print("Test")
})
