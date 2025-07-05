#' @title Add a canvas specification to a ggplot object
#' @description A canvas specification essentially comprises a set of parameters
#'   from [ggplot2::ggsave()]. When a plot with this canvas specification is
#'   printed, it is rendered as it would appear if saved to a file with the
#'   specified dimensions.
#'
#' @inheritParams ggplot2::ggsave
#' 
#' @return An object of class `canvas` that can be added to a `ggplot` object
#'   to specify the plot dimensions.
#' 
#' @examplesIf rstudioapi::isAvailable()
#' library(ggplot2)
#' p <-
#'   ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ggtitle("My awesome plot")
#'
#' p + canvas(3, 3)
#' p + canvas(5, 3, dpi = 400)
#'
#' @export
canvas <- function(width, height, units = c("in", "cm", "mm", "px"),
                   dpi = 300, scale = 1, bg = "white") {
  structure(
    list(
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      scale = scale,
      bg = bg
    ),
    class = c("canvas", "gg")
  )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.canvas <- function(object, plot, object_name, ...) {
  plot$canvas <- object
  class(plot) <- unique(c("ggview", class(plot)))
  plot
}

#' @export
print.ggview <- function(x, ...) {
  ggview(
    plot = x,
    width = x$canvas$width,
    height = x$canvas$height,
    units = x$canvas$units,
    dpi = x$canvas$dpi,
    scale = x$canvas$scale,
    bg = x$canvas$bg
  )
}

#' @export
plot.ggview <- print.ggview

drop_ggview_class <- function(x) {
  class(x) <- setdiff(class(x), "ggview")
  x
}
