
#' Linear regression model
#'
#' Fits a linear regression model using the least squares method.
#'
#' @param formula A formula describing the regression model.
#' @param data A data frame containing the variables in the formula.
#'
#' @return A linreg object containing the coefficients, fitted values,
#' residuals, standard errors, t-values, p-values and other model information.
#'
#' @importFrom stats pt
#' @export
linreg <- function(formula, data) {

  # Create the design matrix from the formula
  X <- stats::model.matrix(formula, data)
  # Get the name and values of the response variable
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]
  n <- nrow(X)
  p <- ncol(X)
  # Calculate the regression coefficients
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  # Calculate the fitted values
  fitted <- X %*% beta
  # Calculate the difference between actual and fitted values
  residuals <- y - fitted
  # Calculate the residual degrees of freedom
  df <- n - p
  # Estimate the residual variance
  sigma2 <- sum(residuals^2) / df
  # Calculate the variance-covariance matrix of the coefficients
  vcov <- sigma2 * solve(t(X) %*% X)
  # Calculate standard errors of the coefficients
  se <- sqrt(diag(vcov))
  # Calculate t-values for the coefficients
  t_value <- beta / se
  # Calculate p-values for the coefficients
  p_value <- 2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)
  result <- list(
    coefficients = beta,
    fitted.values = fitted,
    residuals = residuals,
    df = df,
    sigma2 = sigma2,
    vcov = vcov,
    std.error = se,
    t.value = t_value,
    p.value = p_value,
    formula = formula,
    data = data
  )
  # Give the result the linreg class
  class(result) <- "linreg"
  return(result)
}

