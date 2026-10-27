
#' Ridge regression using QR decomposition
#'
#' Calculates ridge regression coefficients using
#' an augmented design matrix and QR decomposition.
#'
#' @param X Design matrix, including an intercept if required.
#' @param y Numeric response vector.
#' @param lambda Non-negative ridge penalty.
#' @param penalty Diagonal penalty matrix.
#'
#' @return A vector of ridge regression coefficients.
#' @export
ridgereg_qr <- function(X, y, lambda, penalty) {

  X <- as.matrix(X)
  y <- as.numeric(y)
  penalty <- as.matrix(penalty)

  if (nrow(X) != length(y)) {
    stop("X and y must have compatible dimensions")
  }

  if (ncol(X) != nrow(penalty) ||
      nrow(penalty) != ncol(penalty)) {
    stop("penalty must match the number of columns in X")
  }

  if (!is.numeric(lambda) || length(lambda) != 1 ||
      !is.finite(lambda) || lambda < 0) {
    stop("lambda must be a non-negative number")
  }

  if (any(!is.finite(X)) || any(!is.finite(y)) ||
      any(!is.finite(penalty))) {
    stop("Inputs must contain finite values")
  }

  # Factor the penalty matrix: P = t(C) %*% C
  eigen_P <- eigen(penalty, symmetric = TRUE)

  if (any(eigen_P$values < -1e-10)) {
    stop("penalty must be positive semidefinite")
  }

  C <- diag(sqrt(pmax(eigen_P$values, 0))) %*%
    t(eigen_P$vectors)

  # Augment the design matrix for ridge regression
  A <- rbind(X, sqrt(lambda) * C)
  b <- c(y, rep(0, nrow(C)))

  # QR decomposition and least-squares solution
  decomposition <- qr(A)
  beta <- qr.coef(decomposition, b)

  if (anyNA(beta)) {
    stop("QR decomposition could not determine all coefficients")
  }

  names(beta) <- colnames(X)
  beta
}
