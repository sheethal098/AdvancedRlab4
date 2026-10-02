# AdvancedRlab4

[![R-CMD-check](https://github.com/sheethal098/AdvancedRlab4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sheethal098/AdvancedRlab4/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)


An R package that implements linear regression with an object-oriented
(S3) interface. The package fits a linear model using ordinary least squares
and provides methods for inspecting the fit, including diagnostic plots built
with `ggplot2`.

## Installation

You can install the development version of AdvancedRlab4 from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("sheethal098/AdvancedRlab4")
# or
# install.packages("pak")
pak::pak("sheethal098/AdvancedRlab4")
```

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

To show the two plots side by side (requires the `patchwork` package):

```r
patchwork::wrap_plots(p$residual_plot, p$scale_location_plot, ncol = 2)
```

## Vignette

A vignette with a longer walkthrough is included. To have it available after
installing, build it during installation:

```r
devtools::install_github("sheethal098/AdvancedRlab4", build_vignettes = TRUE)
vignette("AdvancedRlab4")
```

If the vignette has a different name, list the available ones with
`browseVignettes("AdvancedRlab4")`.

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

## License

MIT, see [LICENSE.md](LICENSE.md).
