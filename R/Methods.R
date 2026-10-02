
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


#' Prediction function for linreg models
#'
#' @param object A model object.
#' @param ... Additional arguments.
#'
#' @return Predicted values.
#'
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}


#' Return predictions from a linreg model
#'
#' @param object A linreg model.
#' @param ... Additional arguments.
#'
#' @return A vector containing the fitted values.
#'
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

  # Printing the model call
  cat("Call:\n")

  # Converting the stored call to text
  # and removing unnecessary whitespace
  call_text <- paste(deparse(x$call), collapse = " ")
  call_text <- gsub("[[:space:]]+", " ", call_text)

  # Printing the model call
  cat(call_text, "\n")

  # Printing the heading for the coefficients
  cat("\nCoefficients:\n")

  # Printing the estimated regression coefficients
  print(x$coefficients)

  # Returning the model object invisibly
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

  # Combining the regression results into one coefficient table
  coefficients <- cbind(
    Estimate = object$coefficients,
    `Std. Error` = object$std.error,
    `t value` = object$t.value,
    `Pr(>|t|)` = object$p.value
  )

  # Setting the row names to the coefficient names
  rownames(coefficients) <- names(object$coefficients)

  # Printing the model call
  cat("Call:\n")
  print(object$call)

  # Printing the heading for the coefficient table
  cat("\nCoefficients:\n")

  # Printing the coefficient table with p-values
  # and significance stars
  printCoefmat(
    coefficients,
    P.values = TRUE,
    has.Pvalue = TRUE,
    signif.stars = TRUE
  )

  # Printing the residual standard error and
  # residual degrees of freedom
  cat(
    "\nResidual standard error:",
    sqrt(object$sigma2),
    "on",
    object$df,
    "degrees of freedom\n"
  )

  # Returning the model object invisibly
  invisible(object)
}



#' Diagnostic plots for a linreg model
#'
#' Produces a residuals vs fitted values plot and a scale-location plot.
#' When the model contains a categorical predictor, both plots include a
#' red line connecting the median value for each group. Otherwise, a red
#' loess smoother is drawn instead. The most extreme observations are
#' labelled with their observation number.
#'
#' @param x A linreg model object.
#' @param n_labels Number of the most extreme observations (largest
#'   absolute standardized residual) to label. Defaults to 3.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list containing two ggplot objects:
#'   \code{residual_plot} and \code{scale_location_plot}.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median

plot.linreg <- function(x, n_labels = 3, ...) {

  # Hat values and standardized residuals
  X <- x$X
  XtX_inv <- solve(t(X) %*% X)
  h <- diag(X %*% XtX_inv %*% t(X))
  standardized_residuals <- x$residuals / (sqrt(x$sigma2) * sqrt(1 - h))

  # Categorical predictors
  variables <- all.vars(x$formula)[-1]
  categorical_vars <- variables[
    sapply(x$data[variables], function(z) is.factor(z) || is.character(z))
  ]

  # Data for the plots
  data_plot <- data.frame(
    fitted = as.vector(x$fitted.values),
    residuals = as.vector(x$residuals),
    standardized_residuals = as.vector(standardized_residuals)
  )
  data_plot$sqrt_std <- sqrt(abs(data_plot$standardized_residuals))

  # Observation labels (row names if present, otherwise index)
  obs_names <- names(x$residuals)
  data_plot$id <- if (is.null(obs_names)) seq_len(nrow(data_plot)) else obs_names

  # Label the n most extreme observations
  top <- order(abs(data_plot$standardized_residuals), decreasing = TRUE)[
    seq_len(min(n_labels, nrow(data_plot)))
  ]
  outliers <- data_plot[top, ]

  # Group medians for the red lines
  if (length(categorical_vars) > 0) {
    data_plot$group <- x$data[[categorical_vars[1]]]
    median_data <- aggregate(
      cbind(fitted, residuals, sqrt_std) ~ group,
      data = data_plot,
      FUN = median
    )
  } else {
    median_data <- NULL
  }

  # Text under the x axis, like base R
  model_call <- paste0("lm(", paste(deparse(x$formula), collapse = ""), ")")
  x_lab <- paste0("Fitted values\n", model_call)

  # Shared theme: box around the panel, no grid, no fill
  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "plain", size = 11),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
      axis.title.x = ggplot2::element_text(hjust = 0.5)
    )

  # Red line: group medians, or a loess smoother if there are no groups
  add_red_line <- function(p, yvar) {
    if (!is.null(median_data)) {
      p + ggplot2::geom_line(
        data = median_data,
        ggplot2::aes(x = .data$fitted, y = .data[[yvar]], group = 1),
        color = "red"
      )
    } else {
      p + ggplot2::geom_smooth(
        method = "loess", formula = y ~ x, se = FALSE,
        color = "red", linewidth = 0.5
      )
    }
  }

  # Residuals vs Fitted
  p1 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = .data$fitted, y = .data$residuals)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::geom_point(shape = 1, fill = NA, size = 2)
  p1 <- add_red_line(p1, "residuals") +
    ggplot2::geom_text(
      data = outliers,
      ggplot2::aes(label = .data$id),
      hjust = 1.4, size = 3
    ) +
    ggplot2::labs(x = x_lab, y = "Residuals", title = "Residuals vs Fitted") +
    base_theme

  # Scale-Location
  p2 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(x = .data$fitted, y = .data$sqrt_std)
  ) +
    ggplot2::geom_point(shape = 1, fill = NA, size = 2)
  p2 <- add_red_line(p2, "sqrt_std") +
    ggplot2::geom_text(
      data = outliers,
      ggplot2::aes(label = .data$id),
      hjust = 1.4, size = 3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(
      x = x_lab,
      y = expression(sqrt("|Standardized residuals|")),
      title = "Scale-Location"
    ) +
    base_theme

  list(
    residual_plot = p1,
    scale_location_plot = p2
  )
}
