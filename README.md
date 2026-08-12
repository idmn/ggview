# ggview <img src="man/figures/logo.svg" align="right" width="139" />
-----------

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ggview)](https://CRAN.R-project.org/package=ggview)
[![R-CMD-check](https://github.com/idmn/ggview/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/idmn/ggview/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Choose the right picture size for a ggplot without leaving your IDE.

1. Set picture dimensions with the `canvas()` element.

![](man/figures/ggview.gif)

2. Save the plot with `save_ggplot()`.

```R
p <- 
  ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  ggtitle("My Plot") +
  canvas(800, 1000, units = "px")

save_ggplot(p, "my_plot.png")
```

### Galleries

Use `gallery()` to preview several plots in one Viewer page. A gallery is a
named list of ggplots and nested galleries; nested galleries appear as folders.
The number of columns adapts to the Viewer width; use the thumbnail-size slider
in the bottom-right corner to show more or fewer plots. Click a plot for its
usual `ggview` preview, then use **Back to gallery** to return.

```r
plots <- list(
  scatter = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
  bars = ggplot(mpg, aes(drv)) + geom_bar()
)

g <- as.gallery(list(
  exploratory = plots,
  diagnostics = list(residuals = ggplot(mtcars, aes(wt, mpg)) + geom_point())
))
g$exploratory$scatter # extract an individual ggplot
g # opens a grid preview in the Viewer
```

### Installation

```r
install.packages("ggview")

# development version
remotes::install_github("idmn/ggview")
```

### VS Code

The package relies on the `rstudioapi::viewer()` function to display plot previews. By default it 
does not work in VS Code, but installing the [R extension](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r) fixes it. It tricks the `rstudioapi` package into believing it is running in RStudio, and everything works.
