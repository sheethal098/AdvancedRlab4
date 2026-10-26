test_that("ridgereg returns a correctly structured model", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  expect_s3_class(mod, "ridgereg")
  expect_named(mod$coefficients)
  expect_length(mod$fitted.values, nrow(iris))
  expect_length(mod$residuals, nrow(iris))
})

test_that("ridgereg calculates fitted values correctly", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  expect_equal(
    mod$fitted.values,
    as.vector(mod$X %*% mod$coefficients)
  )

  expect_equal(
    mod$residuals,
    iris$Petal.Length - mod$fitted.values
  )
})

test_that("ridgereg rejects invalid lambda values", {
  formula <- Petal.Length ~ Species

  expect_error(ridgereg(formula, iris, lambda = -1))
  expect_error(ridgereg(formula, iris, lambda = NA_real_))
  expect_error(ridgereg(formula, iris, lambda = Inf))
  expect_error(ridgereg(formula, iris, lambda = "1"))
})

# Tests for coef(), print(), and predict()

test_that("coef returns the ridge coefficients", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  expect_equal(coef(mod), mod$coefficients)
})

test_that("print displays the call and coefficients", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  expect_output(print(mod), "Call")
  expect_output(print(mod), "Coefficients")
})

test_that("predict returns fitted values by default", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  expect_equal(predict(mod), mod$fitted.values)
})

test_that("predict works with new data", {
  mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

  newdata <- iris[1:10, ]
  predictions <- predict(mod, newdata = newdata)

  expect_length(predictions, 10)
  expect_true(all(is.finite(predictions)))
  expect_equal(
    predictions,
    mod$fitted.values[1:10],
    tolerance = 1e-8
  )
})
