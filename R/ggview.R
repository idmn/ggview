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
                   bg = NULL,
                   return_to = NULL) {
  plot <- drop_ggview_class(plot)
  device <- match.arg(device)
  units <- match.arg(units)
  path_dir <- ggview_session()

  preview <- ggview_write_preview(
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg,
    path_dir = path_dir,
    name = basename(tempfile(pattern = "ggview_", tmpdir = path_dir)),
    return_to = return_to
  )
  rstudioapi::viewer(preview$html)
  invisible(plot)
}

ggview_write_preview <- function(plot, device, scale, width, height, units,
                                 dpi, bg, path_dir, name, return_to = NULL,
                                 path = NULL) {
  path_file <- file.path(path_dir, paste0(name, ".", device))
  ggplot2::ggsave(
    filename = path_file,
    plot = drop_ggview_class(plot),
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )

  path_html <- file.path(path_dir, paste0(name, ".html"))
  html_template <- readLines(system.file("ggview.html", package = "ggview"))
  html_content <- gsub("{{file}}", basename(path_file), html_template, fixed = TRUE)
  html_content <- gsub("{{back_button}}", ggview_navigation(return_to, path),
                       html_content, fixed = TRUE)
  html_content <- gsub("{{canvas_info}}", ggview_canvas_info(plot), html_content,
                       fixed = TRUE)
  writeLines(html_content, path_html)
  list(image = path_file, html = path_html)
}

#' @description Creates a directory for one preview session and removes older
#'   sessions from the ggview temporary directory.
#'
#' @noRd
ggview_session <- function(n = 10) {
  path_root <- file.path(tempdir(), "ggview")
  if (!dir.exists(path_root)) dir.create(path_root)

  path_dir <- tempfile(pattern = "ggview_", tmpdir = path_root)
  dir.create(path_dir)
  ggview_cleanup(path_root, n)
  path_dir
}

ggview_cleanup <- function(dir, n = 10) {
  entries <- list.files(dir, full.names = TRUE, no.. = TRUE)
  info <- file.info(entries)
  unlink(entries[!info$isdir], force = TRUE)

  sessions <- entries[info$isdir]
  sessions <- sessions[order(file.info(sessions)$mtime, decreasing = TRUE)]
  unlink(sessions[seq_along(sessions) > n], recursive = TRUE, force = TRUE)
}

ggview_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

ggview_navigation <- function(return_to, path = NULL) {
  if (is.null(return_to)) return("")
  path <- ggview_html_escape(path %||% character())
  crumbs <- c(
    if (length(path) > 1) {
      paste0("<span class=\"back-ancestor\">", utils::head(path, -1), "</span>")
    },
    paste0("<span class=\"back-current\">", utils::tail(path, 1), "</span>")
  )
  paste0(
    "<nav class=\"back-nav\"><a class=\"back\" href=\"", basename(return_to),
    "\">Back</a><span class=\"back-path\">",
    paste(crumbs, collapse = "<span class=\"back-separator\">/</span>"), "</span></nav>"
  )
}

ggview_canvas_info <- function(plot) {
  canvas <- plot$canvas
  if (is.null(canvas)) return("")

  background <- canvas$bg %||% "theme background"
  paste0(
    "<div class=\"canvas-info\"><button class=\"canvas-icon\" type=\"button\">i</button><div class=\"canvas-card\">",
    "<strong>Canvas</strong><div>", ggview_html_escape(canvas$width), " &times; ",
    ggview_html_escape(canvas$height), " ", ggview_html_escape(canvas$units),
    "</div><div>", ggview_html_escape(canvas$dpi), " dpi &middot; scale ",
    ggview_html_escape(canvas$scale),
    "</div><div>", ggview_html_escape(paste(background, "background")),
    "</div></div></div>"
  )
}
