
test_that("QR matches the direct ridge solution", {
  data <- data.frame(
    x = 1:10,
    y = c(2.1, 3.8, 6.2, 7.9, 10.2,
          11.8, 14.1, 15.9, 18.2, 19.8)
  )

  X <- model.matrix(y ~ x, data = data)
  y <- data$y
  P <- diag(c(0, 1))
  lambda <- 2

  beta_qr <- ridgereg_qr(X, y, lambda, P)
  beta_direct <- solve(
    crossprod(X) + lambda * P,
    crossprod(X, y)
  )

  expect_equal(
    unname(beta_qr),
    unname(as.vector(beta_direct)),
    tolerance = 1e-8
  )
})

test_that("QR works with multiple predictors", {
  set.seed(123)
  data <- data.frame(x1 = rnorm(30), x2 = rnorm(30))
  data$y <- 2 + 3 * data$x1 - data$x2 + rnorm(30)

  X <- model.matrix(y ~ x1 + x2, data = data)
  P <- diag(c(0, 1, 1))
  lambda <- 1.5

  beta <- ridgereg_qr(X, data$y, lambda, P)

  expect_length(beta, 3)
  expect_named(beta, colnames(X))
  expect_true(all(is.finite(beta)))
})

test_that("zero penalty agrees with ordinary least squares", {
  X <- cbind(Intercept = 1, x = 1:10)
  y <- 2 + 3 * (1:10) + c(0.1, -0.2, 0.1, 0, 0.2,
                          -0.1, 0.1, -0.2, 0.2, 0)

  P <- diag(c(0, 1))
  beta <- ridgereg_qr(X, y, 0, P)

  expect_equal(
    unname(beta),
    unname(as.vector(qr.solve(X, y))),
    tolerance = 1e-8
  )
})

test_that("QR rejects incompatible dimensions", {
  X <- cbind(1, 1:5)
  y <- 1:4
  P <- diag(2)

  expect_error(
    ridgereg_qr(X, y, 1, P),
    "compatible dimensions"
  )
})

test_that("QR rejects a mismatched penalty matrix", {
  X <- cbind(1, 1:5)
  y <- 1:5
  P <- diag(3)

  expect_error(
    ridgereg_qr(X, y, 1, P),
    "penalty must match"
  )
})

test_that("QR rejects negative lambda", {
  X <- cbind(1, 1:5)
  y <- 1:5
  P <- diag(c(0, 1))

  expect_error(
    ridgereg_qr(X, y, -1, P),
    "non-negative"
  )
})

test_that("QR rejects non-finite inputs", {
  X <- cbind(1, c(1, 2, NA, 4, 5))
  y <- 1:5
  P <- diag(c(0, 1))

  expect_error(
    ridgereg_qr(X, y, 1, P),
    "finite values"
  )
})

test_that("QR rejects a non-positive-semidefinite penalty", {
  X <- cbind(1, 1:5)
  y <- 1:5
  P <- diag(c(0, -1))

  expect_error(
    ridgereg_qr(X, y, 1, P),
    "positive semidefinite"
  )
})

test_that("QR handles lambda equal to zero", {
  X <- cbind(Intercept = 1, x = 1:8)
  y <- c(2, 3, 5, 6, 8, 9, 11, 12)
  P <- diag(c(0, 1))

  beta <- ridgereg_qr(X, y, 0, P)

  expect_true(all(is.finite(beta)))
  expect_length(beta, 2)
})
