context("ridgereg")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Spaces, data=iris, lambda = 1))
  expect_error(ridgereg(Petal.Length~Species, data=iris, lambda = 2, QR_dec = 27))
  expect_error(ridgereg(Petal.Length~Species, data=iris, lambda = "59"))
})


test_that("Same results for predict() with qr", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  ridgereg_mod_qr <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, QR_dec = TRUE)
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]),2), c(1.82, 1.51, 1.07))    
  expect_equal(round(unname(ridgereg_mod_qr$predict()[c(1,5,7)]),2), c(1.82, 1.51, 1.07))
})


test_that("Test of coef() results vs lm.ridge", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  ridgereg_mod_qr <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, QR_dec = TRUE)
  
  iris_scaled <- iris
  iris_scaled$Sepal.Width <- scale(iris_scaled$Sepal.Width)
  iris_scaled$Sepal.Length <- scale(iris_scaled$Sepal.Length)
  ridgereg_lm <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris_scaled, lambda = 2)
  
  expect_true(all(ceiling(ridgereg_mod$coef()) == ceiling(coef(ridgereg_lm))))
  expect_true(all(ceiling(ridgereg_mod_qr$coef()) == ceiling(coef(ridgereg_lm))))
  expect_true(all(round(ridgereg_mod$coef(),2) == round(ridgereg_mod_qr$coef(), 2)))
})