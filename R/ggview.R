#' @title Ggplot picture previewer
#' @description Preview what a ggplot would look like is you save it to a file.
#'   Set picture parameters as you would set them in [ggplot2::ggsave] and see
#'   the result in RStudio viewer.
#'
#' @param device Device to use. Can be one of "png", "jpeg", "bmp" or "svg".
#' @inheritParams ggplot2::ggsave
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <-
#'   ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ggtitle("My awesome plot")
#'
#' ggview(width = 3, height = 3)
#' ggview(width = 5, height = 3)
#' }
#'
#' @export
ggview <- function(plot = ggplot2::last_plot(),
                   device = c("png", "jpeg", "bmp", "svg"),
                   scale = 1,
                   width,
                   height,
                   units = c("in", "cm", "mm", "px"),
                   dpi = 300,
                   bg = NULL
                   ) {

  device <- match.arg(device)
  path_dir <- file.path(tempdir(), "ggview")
  if (!dir.exists(path_dir)) dir.create(path_dir)
  path_file <- file.path(path_dir, paste0("plot.", device))

  ggplot2::ggsave(
    filename = path_file,
    plot = plot,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )

  path_html <- file.path(path_dir, "ggview.html")
  html_template <- readLines(system.file("ggview.html", package = "ggview"))
  html_content <- gsub("{{file}}", basename(path_file), html_template, fixed = TRUE)
  writeLines(html_content, path_html)
  rstudioapi::viewer(path_html)
  invisible(plot)
}
