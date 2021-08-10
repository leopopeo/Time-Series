test_that("Eingabe testen", {
  expect_error(DLA(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NA), "Eingabe ist nicht numerisch!")
  expect_error(DLA(NULL), "Eingabe ist nicht numerisch!")
  expect_error(DLA(c(1)), "Die Laenge des Vektors muss groeßer als 1 sein!")

  expect_error(DLA(c(3,4,5,1), len=1.1), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=c(1,1)), "len muss >= 2 sein!")
  expect_error(DLA(c(3,4,5,1), len=NA), "len muss NULL oder ein Integer Wert sein!")
  expect_error(DLA(c(3,4,5,1), len="a"), "len muss NULL oder ein Integer Wert sein!")
})

test_that("Durbin Levinson Algorithmus Implementation funktioniert", {
  #Test der for-Schleife:
  #Vorbereitung:
  set.seed(1)
  x <- c(0.4,0.5,0.3)
  acf_x <- c(ACF(x),0)
  phi <- double(3)
  #erste Iteration
  phi_11 <- acf_x[2]/acf_x[1]
  v_1 <- acf_x[1]*(1-phi_11^2)
  #zweite Iteration
  phi_22 <- (acf_x[2]-phi_11*acf_x[2])/v_1
  phi_21 <- phi_11-phi_22*phi_11
  v_2 <- v_1*(1-phi_22^2)
  #dritte Iteration
  phi[3] <- (acf_x[3]-sum(phi_21*acf_x[3], phi_22*acf_x[2]))/v_2
  phi[1] <- phi_21-phi[3]*phi_22
  phi[2] <- phi_22-phi[3]*phi_21

  expect_equal(DLA(x)[1:3], phi)
})
