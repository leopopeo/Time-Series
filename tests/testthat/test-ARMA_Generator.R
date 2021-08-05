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
