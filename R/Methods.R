#' Extract coefficients from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A named vector containing the regression coefficients.
#'
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}


#' Extract residuals from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A vector containing the model residuals.
#'
#' @export
#' @importFrom stats resid

resid.linreg <- function(object, ...) {
  object$residuals
}


#' Return predictions from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A vector containing the fitted values.
#'
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}


#' @export
pred.linreg <- function(object, ...) {
  object$fitted.values
}


#' Print a linreg model
#'
#' @param x A linreg model.
#' @param ... Additional arguments.
#'
#' @return The model object, invisibly.
#'
#' @export
print.linreg <- function(x, ...) {

  cat("Call:\n")
  call_text <- paste(deparse(x$call), collapse = " ")
  call_text <- gsub("[[:space:]]+", " ", call_text)

  cat(call_text, "\n")


  cat("\nCoefficients:\n")
  print(x$coefficients)

  invisible(x)
}

#' Summarize a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return The model object, invisibly.
#'
#' @export
#' @importFrom stats printCoefmat
summary.linreg <- function(object, ...) {

  # Put the coefficient results into one numeric table
  coefficients <- cbind(
    Estimate = object$coefficients,
    `Std. Error` = object$std.error,
    `t value` = object$t.value,
    `Pr(>|t|)` = object$p.value
  )

  rownames(coefficients) <- names(object$coefficients)

  cat("Call:\n")
  print(object$call)

  cat("\nCoefficients:\n")
  printCoefmat(
    coefficients,
    P.values = TRUE,
    has.Pvalue = TRUE,
    signif.stars = TRUE
  )

  cat(
    "\nResidual standard error:",
    sqrt(object$sigma2),
    "on",
    object$df,
    "degrees of freedom\n"
  )

  invisible(object)
}

#' Plot a linreg model
#'
#' Produces Residuals vs Fitted and Scale-Location plots.
#'
#' @param x A linreg model.
#' @param ... Additional arguments.
#'
#' @return A list containing two ggplot objects.
#'
#' @export
#' @importFrom rlang .data
plot.linreg <- function(x, ...) {

  # Get the design matrix
  X <- x$X

  # Calculate the hat values
  XtX_inv <- solve(t(X) %*% X)
  h <- diag(X %*% XtX_inv %*% t(X))

  # Calculate standardized residuals
  standardized_residuals <- x$residuals /
    (sqrt(x$sigma2) * sqrt(1 - h))

  # Store the values needed for the plots
  data_plot <- data.frame(
    fitted = as.vector(x$fitted.values),
    residuals = as.vector(x$residuals),
    standardized_residuals = standardized_residuals
  )

  # Residuals vs Fitted plot
  p1 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = .data$fitted, y = .data$residuals)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    )

  # Scale-Location plot
  p2 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(
      x = .data$fitted,
      y = sqrt(abs(.data$standardized_residuals))
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Fitted values",
      y = "Sqrt(|Standardized residuals|)",
      title = "Scale-Location"
    )

  # Return both plots
  list(
    residual_plot = p1,
    scale_location_plot = p2
  )
}
