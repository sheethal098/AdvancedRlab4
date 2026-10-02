
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


#' Extract residuals from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return The model residuals.
#' @export
#' @importFrom stats resid
resid.linreg <- function(object, ...) {
  object$residuals
}


#' Prediction function for linreg models
#'
#' @param object A model object.
#' @param ... Additional arguments.
#'
#' @return Predicted values.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}


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


#' Summarize a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A summary of the regression model.
#' @export
summary.linreg <- function(object, ...) {

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

  cat("\nResidual standard error:", sqrt(object$sigma2), "\n")
  cat("Degrees of freedom:", object$df, "\n")

  invisible(object)
}


#' Plot a linreg model
#'
#' @param x A linreg model.
#' @param ... Additional arguments.
#'
#' @return A list containing two ggplot objects.
#' @importFrom stats model.frame model.matrix model.response
#' @export

plot.linreg <- function(x, ...) {

  # Get the response variable from the original data
  observed <- model.response(model.frame(x$formula, x$data))

  # Get predictor values
  predictor <- model.matrix(x$formula, x$data)[, 2]

  # Create a data frame for plotting
  data_plot <- data.frame(
    predictor = predictor,
    observed = observed,
    fitted_values = as.vector(x$fitted.values),
    residual_values = as.vector(x$residuals)
  )

  # Regression plot
  p1 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = predictor, y = observed)
  )
    ggplot2::geom_point()
    ggplot2::geom_abline(
      intercept = x$coefficients[1],
      slope = x$coefficients[2]
    )
    ggplot2::labs(
      x = all.vars(x$formula)[2],
      y = all.vars(x$formula)[1],
      title = "Linear Regression"
    )
  # Residual plot
  p2 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = fitted_values, y = residual_values)
  )
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    )
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    )
  list(
    regression_plot = p1,
    residual_plot = p2
  )
}
