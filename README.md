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

### Collections of plots

A script that makes forty plots needs somewhere to put them. `plots_init()`
starts a collection, and `plots_append()` adds to it. Each plot keeps its name,
its type, the canvas size it should be saved at, and whatever anyone has
changed about it.

```r
plots <- plots_init(name = "Engine notes")
plots <- plots_append(plots, scatter, name = "Engine/weight", type = "point")
plots <- plots_append(plots, bars, name = "Engine/cylinders", type = "bar")

plots                                  # lists what is in the collection
plots_get(plots, "Engine/weight")      # one plot, at its own size
save_plots(plots, dir = "plots")       # one file per plot, folders included
as.gallery(plots)                      # browse them all in the Viewer
```

Every name in that listing is a link: click one and the plot opens in the
Viewer at its own size. `as_tibble(plots)` gives the plain tibble instead of the
listing.

A change is kept beside the plot rather than written into it, so it can always
be undone and the plot never has to be rebuilt:

```r
plots <- plots_set(plots, "Engine/weight", title = "Weight drives mileage",
                   width = 8, height = 5)
plots <- plots_reset(plots, "Engine/weight")     # back to what it was

# Re-size every bar chart at once; it is an ordinary tibble.
plots <- plots |> dplyr::mutate(height = ifelse(type == "bar", 6, height))
```

What a plot lets you change is its own business. `customizer_default()` offers
the labels, gridlines and category colors every plot has; write a `customizer()`
for anything else, give it to
`plots_init()` under a name, and let each plot name the one it uses. Functions
the customizers share go to `plots_init(functions = )`, so they are written once.
`plots_params()` says what any given plot accepts — enough for a program to
build a control for each one.

```r
plots_params(plots, "Engine/weight")
#> ── <plots_params> ─────────────────────────────────── 8 parameters ──
#>   title    text   Heavier cars use more fuel
#>   subtitle text   N = 32
#>   ...
#>   width    number 15
```

See `vignette("plots")`.

### Installation

```r
install.packages("ggview")

# development version
remotes::install_github("idmn/ggview")
```

### VS Code

The package relies on the `rstudioapi::viewer()` function to display plot previews. By default it 
does not work in VS Code, but installing the [R extension](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r) fixes it. It tricks the `rstudioapi` package into believing it is running in RStudio, and everything works.
