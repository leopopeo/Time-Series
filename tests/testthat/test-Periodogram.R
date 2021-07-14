test_that("Eingabe testen", {
  expect_error(perio(c("a","b")), "Eingabe ist nicht numerisch!")
  expect_error(perio(NA), "Eingabe ist nicht numerisch!")
  expect_error(perio(c(1)), "Die Länge des Vektors muss größer als 1 sein!")
})

test_that("Periodogram Algorithmus funktioniert", {
  #Test der implementation über die doppelte For-Schleife
  #Vorbereitung
  x <- c(5,8)
  n <- length(x)
  F_n <- c(-1,0) #Ausgerechnet über -(n-1)/2):(n/2) wie es im Brockwell steht
  test <- double(2)

  test[1] <- 1/n*abs(x[1]*exp(as.complex(-1i)*1*(2*pi*F_n[1])/n)+x[2]*exp(as.complex(-1i)*2*(2*pi*F_n[1])/n))**2
  test[2] <- 1/n*abs(x[1]*exp(as.complex(-1i)*1*(2*pi*F_n[2])/n)+x[2]*exp(as.complex(-1i)*2*(2*pi*F_n[2])/n))**2

  expect_equal(perio(x),test)
})
