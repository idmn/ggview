#' @title Ggplot picture previewer
#' @description Preview what a ggplot would look like if you save it to a file.
#'   Set picture parameters as you would set them in [ggplot2::ggsave] and see
#'   the result in RStudio viewer.
#'
#' @param device Device to use. Can be one of "png", "jpeg", "bmp" or "svg".
#' @inheritParams ggplot2::ggsave
#' @noRd
ggview <- function(plot = ggplot2::last_plot(),
                   device = c("png", "jpeg", "bmp", "svg"),
                   scale = 1,
                   width,
                   height,
                   units = c("in", "cm", "mm", "px"),
                   dpi = 300,
                   bg = NULL
                   ) {
  plot <- drop_ggview_class(plot)

  device <- match.arg(device)
  units <- match.arg(units)
  path_dir <- file.path(tempdir(), "ggview")

  if (!dir.exists(path_dir)) dir.create(path_dir)
  list.files(path_dir)
  ggview_cleanup(path_dir)

  random_name <- tempfile(pattern = "ggview_", tmpdir = path_dir)
  path_file <- paste0(random_name, paste0(".", device))

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

  path_html <- paste0(random_name, ".html")
  html_template <- readLines(system.file("ggview.html", package = "ggview"))
  html_content <- gsub("{{file}}", basename(path_file), html_template, fixed = TRUE)
  writeLines(html_content, path_html)
  rstudioapi::viewer(path_html)
  invisible(plot)
}

#' @description Removes all plots+html pairs in the ggview tmp directory but the
#'   n most recent ones.
#'
#' @noRd
ggview_cleanup <- function(dir, n = 50) {
  file_info <- file.info(list.files(dir, full.names = TRUE))
  file_info <- file_info[order(file_info$ctime, decreasing = TRUE),]
  files_sorted <- rownames(file_info)
  to_remove <- files_sorted[seq_along(files_sorted) > n]
  file.remove(to_remove)
}
