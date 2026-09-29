# Extract coefficients from the model
# This allows coef(model) to return the coefficients
#' Extract coefficients from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return The regression coefficients.
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}


# Extract residuals from the model
# This allows resid(model) to return the residuals
#' Extract residuals from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return The model residuals.
#' @export
resid.linreg <- function(object, ...) {
  object$residuals
}


# Create the pred() function for different model types
pred <- function(object, ...) {
  UseMethod("pred")
}


# Return fitted values as predictions
#' Return predictions from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return The fitted values from the model.
#' @export
pred.linreg <- function(object, ...) {
  object$fitted.values
}

# Connect pred() to pred.linreg() for linreg models
registerS3method("pred", "linreg", pred.linreg)


# Print the main information from the model
#' Print a linreg model
#'
#' @param x A linreg model.
#' @param ... Additional arguments.
#'
#' @return The object invisibly.
#' @export
print.linreg <- function(x, ...) {
  cat("Linear Regression Model\n")
  cat("Formula:", deparse(x$formula), "\n\n")
  cat("Coefficients:\n")
  print(x$coefficients)

  invisible(x)
}


# Show the main results from the model
#' Summarize a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A summary of the regression model.
#' @export
summary.linreg <- function(object, ...) {

  # Put the coefficient results into one table
  coefficients <- cbind(
    Estimate = as.vector(object$coefficients),
    `Std. Error` = as.vector(object$std.error),
    `t value` = as.vector(object$t.value),
    `Pr(>|t|)` = as.vector(object$p.value)
  )

  rownames(coefficients) <- rownames(object$coefficients)

  cat("Linear Regression Model\n")
  cat("Formula:", deparse(object$formula), "\n\n")

  cat("Coefficients:\n")
  print(coefficients)

  # Show the error and degrees of freedom
  cat("\nResidual standard error:", sqrt(object$sigma2), "\n")
  cat("Degrees of freedom:", object$df, "\n")

  invisible(object)
}


# Connect summary() to summary.linreg() for linreg models
registerS3method("summary", "linreg", summary.linreg)


# Create plots for the regression model
#' Plot a linreg model
#'
#' @param x A linreg model.
#' @param ... Additional arguments.
#'
#' @return A list of ggplot objects.
#' @export
plot.linreg <- function(x, ...) {

  # Get the actual response values from the model
  observed <- model.response(model.frame(x$formula, x$data))

  # Get the predictor values used in the model
  predictor <- model.matrix(x$formula, x$data)[, 2]

  # Store the values needed for both plots
  data_plot <- data.frame(
    predictor = predictor,
    observed = observed,
    fitted = as.vector(x$fitted.values),
    residuals = as.vector(x$residuals)
  )

  # First plot: actual data and the fitted regression line
  p1 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = predictor, y = observed)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      intercept = x$coefficients[1],
      slope = x$coefficients[2]
    ) +
    ggplot2::labs(
      x = all.vars(x$formula)[2],
      y = all.vars(x$formula)[1],
      title = "Linear Regression"
    )

  # Second plot: residuals compared with fitted values
  p2 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = fitted, y = residuals)
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

  # Return both plots
  list(
    regression_plot = p1,
    residual_plot = p2
  )
}


# Connect plot() to plot.linreg() for linreg models
registerS3method("plot", "linreg", plot.linreg)
