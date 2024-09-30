#' @title Save a ggplot
#' @description Saves a ggplot object just like [ggplot2::ggsave()]. If the plot
#'   has a [canvas()] specified, these canvas parameters are used.
#'   User-specified parameters will override the canvas defaults.
#' @inheritParams ggplot2::ggsave
#' @param plot The ggplot object to save.
#' @param file File to save the plot to.
#' @examples
#' library(ggplot2)
#' p <-
#'   ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ggtitle("My awesome plot") +
#'   canvas(8, 6)
#'
#' \dontrun{
#' temp_file <- tempfile(fileext = ".png")
#' save_ggplot(p, temp_file)
#' }
#'
#' @export
save_ggplot <- function(plot, file,
                        device = NULL,
                        scale = NULL,
                        width = NULL, height = NULL,
                        units = NULL,
                        dpi = NULL, limitsize = TRUE,
                        bg = NULL, create.dir = FALSE,
                        ...) {

  canvas_params <- plot$canvas

  scale <- scale %||% canvas_params$scale %||% 1
  width <- width %||% canvas_params$width %||% NA
  height <- height %||% canvas_params$height %||% NA
  units <- units %||% canvas_params$units %||% "in"
  dpi <- dpi %||% canvas_params$dpi %||% 300
  bg <- bg %||% canvas_params$bg %||% NULL


  ggplot2::ggsave(
    filename = file,
    plot = drop_ggview_class(plot),
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg,
    create.dir = create.dir,
    ...
  )
}
