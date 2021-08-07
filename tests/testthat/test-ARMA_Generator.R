test_that("Eingabe testen", {
  #Phi
  expect_error(arma_sim(phi=c("a"),sd=1,I=100), "Phi muss ein numerischer Vektor oder NULL sein.")
  expect_error(arma_sim(phi=NA,sd=1,I=100), "Phi muss ein numerischer Vektor oder NULL sein.")
  expect_error(arma_sim(phi = 10, sd = 1, I = 100), "AR-Teil muss stationaer sein.")
  #Theta
  expect_error(arma_sim(theta=c("a"),sd=1,I=100), "Theta muss ein numerischer Vektor oder NULL sein.")
  expect_error(arma_sim(theta=NA,sd=1,I=100), "Theta muss ein numerischer Vektor oder NULL sein.")

  #sd
  expect_error(arma_sim(theta=0.,sd="a",I=100), "Die Standardabweichung sd muss ein numerischer Vektor der Länge 1 sein.")
  expect_error(arma_sim(theta=0.,sd=c(1,1),I=100), "Die Standardabweichung sd muss ein numerischer Vektor der Länge 1 sein.")
  expect_error(arma_sim(theta=0.,sd=NA,I=100), "Die Standardabweichung sd muss ein numerischer Vektor der Länge 1 sein.")

  #I
  expect_error(arma_sim(theta=0.,sd=0.5,I="a"), "I muss ein numerischer Vektor der Länge 1 sein" )
  expect_error(arma_sim(theta=0.,sd=0.5,I=c(1,1)), "I muss ein numerischer Vektor der Länge 1 sein" )
  expect_error(arma_sim(theta=0.,sd=0.5,I=NA), "I muss ein numerischer Vektor der Länge 1 sein" )
  expect_error(arma_sim(theta=0.,sd=0.5,I=1.2), "I muss ein Interger Wert sein")
  expect_error(arma_sim(theta=0.,sd=0.5,I=-1), "I muss größer als 0 sein")
})

test_that("AR works", {

expect_equal(2*2, 4)
})
