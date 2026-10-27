#' Ridge regression model
#'
#' Fits a ridge regression model using a penalty parameter lambda.
#'
#' @param formula A formula describing the regression model.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A non-negative number specifying the penalty strength.
#'
#' @return An object of class "ridgereg" containing coefficients,
#'   fitted values, residuals, and model information.
#' @export
#' @importFrom stats na.fail sd setNames
#' @importFrom stats model.frame model.matrix model.response terms
ridgereg <- function(formula, data, lambda) {

  # Check inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.numeric(lambda) || length(lambda) != 1 ||
      is.na(lambda) || !is.finite(lambda) || lambda < 0) {
    stop("lambda must be a single non-negative number")
  }

  # Create model frame and extract response
  mf <- model.frame(formula, data = data, na.action = na.fail)
  model_terms <- attr(mf, "terms")
  y <- model.response(mf)

  if (!is.numeric(y) || is.matrix(y) ||
      anyNA(y) || any(!is.finite(y))) {
    stop("response must contain finite numeric values")
  }

  # Create the original design matrix
  X_raw <- model.matrix(model_terms, data = mf)

  if (anyNA(X_raw) || any(!is.finite(X_raw))) {
    stop("predictors must contain finite values")
  }

  # Identify the intercept and predictor columns
  intercept <- which(colnames(X_raw) == "(Intercept)")
  predictors <- setdiff(seq_len(ncol(X_raw)), intercept)

  # Save scaling information for prediction on new data
  means <- setNames(numeric(length(predictors)),
                    colnames(X_raw)[predictors])
  sds <- means

  X <- X_raw

  for (j in predictors) {
    column_name <- colnames(X_raw)[j]
    means[column_name] <- mean(X_raw[, j])
    sds[column_name] <- sd(X_raw[, j])

    if (!is.finite(sds[column_name]) || sds[column_name] == 0) {
      stop("predictor columns must have non-zero variance")
    }

    X[, j] <- (X_raw[, j] - means[column_name]) / sds[column_name]
  }

  # Create penalty matrix
  p <- ncol(X)
  penalty <- diag(p)

  if (length(intercept) > 0) {
    penalty[intercept, intercept] <- 0
  }

  # Calculate ridge coefficients
  beta <- solve(
    crossprod(X) + lambda * penalty,
    crossprod(X, y)
  )

  beta <- as.vector(beta)
  names(beta) <- colnames(X)

  # Calculate fitted values and residuals
  fitted <- as.vector(X %*% beta)
  residuals <- as.vector(y - fitted)

  # Save factor levels for prediction
  xlevels <- lapply(
    mf[vapply(mf, is.factor, logical(1))],
    levels
  )

  # Store model results
  result <- list(
    coefficients = beta,
    fitted.values = fitted,
    residuals = residuals,
    lambda = lambda,
    formula = formula,
    terms = model_terms,
    data = data,
    X = X,
    means = means,
    sds = sds,
    xlevels = xlevels,
    contrasts = attr(X_raw, "contrasts"),
    call = match.call()
  )

  class(result) <- "ridgereg"
  result
}
