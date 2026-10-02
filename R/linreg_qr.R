#' Linear regression model using QR decomposition
#'
#' Fits a multiple linear regression model using QR decomposition.
#'
#' @param formula A formula describing the regression model.
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class `linreg` containing the regression
#' coefficients, fitted values, residuals, degrees of freedom,
#' residual variance, variance-covariance matrix, standard errors,
#' t-values, p-values, formula, data, and design matrix.
#'
#' @export
#' @importFrom stats model.matrix pt
linreg_qr <- function(formula, data) {

  # Creating the design matrix
  X <- model.matrix(formula, data)

  # Extracting the response variable
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  # Number of observations and parameters
  n <- nrow(X)
  p <- ncol(X)

  # QR decomposition
  qr_X <- qr(X)

  Q <- qr.Q(qr_X)
  R <- qr.R(qr_X)

  # Calculating regression coefficients
  beta <- solve(R, t(Q) %*% y)

  # Converting coefficients to a named vector
  beta <- as.vector(beta)
  names(beta) <- colnames(X)

  # Calculating fitted values
  fitted <- as.vector(X %*% beta)

  # Calculating residuals
  residuals <- y - fitted

  # Calculating residual degrees of freedom
  df <- n - p

  # Calculating residual variance
  sigma2 <- sum(residuals^2) / df

  # Calculating variance-covariance matrix
  R_inv <- solve(R)
  vcov <- sigma2 * R_inv %*% t(R_inv)

  # Calculating standard errors
  std.error <- sqrt(diag(vcov))
  names(std.error) <- names(beta)

  # Calculating t-values
  t.value <- beta / std.error

  # Calculating two-sided p-values
  p.value <- 2 * pt(
    abs(t.value),
    df = df,
    lower.tail = FALSE
  )

  # Storing model results
  result <- list(
    coefficients = beta,
    fitted.values = fitted,
    residuals = residuals,
    df = df,
    sigma2 = sigma2,
    vcov = vcov,
    std.error = std.error,
    t.value = t.value,
    p.value = p.value,
    formula = formula,
    data = data,
    X = X,
    call = match.call()
  )

  # Assigning the linreg class
  class(result) <- "linreg"

  return(result)
}
