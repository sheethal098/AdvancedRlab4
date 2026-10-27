# AdvancedRlab4

[![R-CMD-check](https://github.com/sheethal098/AdvancedRlab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sheethal098/AdvancedRlab4/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)


An R package that implements linear and ridge regression with an
object-oriented (S3) interface. The package fits linear models using ordinary
least squares or QR decomposition, fits ridge regression models with a
penalty parameter `lambda`, and provides methods for inspecting the fits,
including diagnostic plots built with `ggplot2`. It also includes a function
that visualizes average flight delays at New York City airports.

## Installation

You can install the development version of AdvancedRlab4 from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("sheethal098/AdvancedRlab4")
# or
# install.packages("pak")
pak::pak("sheethal098/AdvancedRlab4")
```

## Overview

| Function | Description |
|---|---|
| `linreg()` | Linear regression using ordinary least squares. |
| `linreg_qr()` | Linear regression using QR decomposition. |
| `ridgereg()` | Ridge regression with penalty `lambda`. |
| `ridgereg_qr()` | Ridge coefficients via an augmented design matrix and QR decomposition. |
| `visualize_airport_delays()` | Average departure delays for NYC airports. |

Methods for `linreg` objects: `print()`, `summary()`, `coef()`, `resid()`,
`pred()` and `plot()`. Methods for `ridgereg` objects: `print()`, `coef()` and
`predict()`.

## Usage

```r
library(AdvancedRlab4)

# Fit a linear model
fit <- linreg(Petal.Length ~ Species, data = iris)

# Inspect the fit
print(fit)
summary(fit)
coef(fit)
resid(fit)
pred(fit)
```

### QR decomposition: `linreg_qr()`

`linreg_qr()` fits the same model as `linreg()`, but solves the least squares
problem with a QR decomposition of the design matrix instead of the normal
equations. This is numerically more stable, especially when the predictors
are highly correlated. It is called in the same way:

```r
fit_qr <- linreg_qr(Petal.Length ~ Species, data = iris)

print(fit_qr)
summary(fit_qr)
coef(fit_qr)
resid(fit_qr)
pred(fit_qr)
```

### Diagnostic plots

`plot()` returns a list with two `ggplot2` objects: a Residuals vs Fitted plot
and a Scale-Location plot. The most extreme observations are labelled with
their observation number, and the number of labels can be changed with
`n_labels`.

```r
p <- plot(fit)

p$residual_plot
p$scale_location_plot

# Label the 5 most extreme observations
p5 <- plot(fit, n_labels = 5)
```

### Ridge regression: `ridgereg()`

`ridgereg()` fits a ridge regression model. The predictors are standardized
before fitting and the intercept is not penalized. With `lambda = 0` the
result is ordinary least squares, and larger values shrink the coefficients
towards zero.

```r
ridge_fit <- ridgereg(Petal.Length ~ ., data = iris, lambda = 1)

print(ridge_fit)
coef(ridge_fit)
```

`predict()` applies the same standardization that was used when fitting, so
new data can be given on the original scale:

```r
predict(ridge_fit, newdata = head(iris))
```

`ridgereg()` can also be used as a custom model in `caret` to choose `lambda`
by cross-validation. This is shown in the `ridgereg` vignette.

### Flight delays

`visualize_airport_delays()` computes the average departure delay for each
NYC origin airport (using `nycflights13`) and plots the airports by location,
coloured by delay. It returns a `ggplot2` object.

```r
visualize_airport_delays()
```

## Vignettes

Vignettes with longer walkthroughs are included. To have them available after
installing, build them during installation:

```r
devtools::install_github("sheethal098/AdvancedRlab4", build_vignettes = TRUE)
browseVignettes("AdvancedRlab4")
```

- **ridgereg**: a prediction problem on the `BostonHousing` data using
  `caret`. It compares linear regression, linear regression with forward
  selection of covariates, and ridge regression with `lambda` chosen by
  10-fold cross-validation. Open it with `vignette("ridgereg")`.

The other vignettes are listed by `browseVignettes("AdvancedRlab4")`.

## Development

Clone the repository and use `devtools`:

```r
devtools::load_all()    # load the package
devtools::document()    # regenerate documentation
devtools::test()        # run the tests
devtools::check()       # run R CMD check
```

The badge at the top of this file shows the result of the latest
`R-CMD-check` GitHub Actions run on the `main` branch: green means the checks
pass, red means they fail.

## Authors

- Sheethal Lakshmana
- Mariam Yayloyan (maintainer)

## License

MIT, see [LICENSE.md](LICENSE.md).
