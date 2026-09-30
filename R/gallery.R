#' Create a gallery
#'
#' A gallery is a named list of ggplots and other galleries. Printing one opens
#' a browsable preview in the RStudio viewer. Names label its entries; nested
#' galleries act as folders.
#'
#' @param ... Gallery entries: ggplots, galleries, or lists that can be
#'   converted to galleries.
#'
#' @return A list of class `gallery`.
#'
#' @examplesIf rstudioapi::isAvailable()
#' library(ggplot2)
#' gallery(
#'   scatter = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'   diagnostics = list(qq = ggplot(mtcars, aes(sample = mpg)) + stat_qq())
#' )
#'
#' @export
gallery <- function(...) {
  new_gallery(lapply(list(...), as_gallery_entry))
}

#' Convert a list of plots to a gallery
#'
#' Plain lists become galleries recursively. Data frames are not treated as
#' lists of plots. A plots collection becomes a gallery whose folders are the
#' slashes in its plot names.
#'
#' @param x A ggplot, gallery, plots collection, or nested plain list of them.
#' @param ... Passed on to methods.
#'
#' @return A gallery.
#'
#' @examples
#' library(ggplot2)
#' invisible(as.gallery(list(scatter = ggplot(mtcars, aes(wt, mpg)) + geom_point())))
#'
#' @export
as.gallery <- function(x, ...) {
  UseMethod("as.gallery")
}

#' @export
as.gallery.default <- function(x, ...) {
  if (inherits(x, "gallery")) {
    return(x)
  }
  if (inherits(x, "ggplot")) {
    return(gallery(x))
  }
  if (!is.list(x) || inherits(x, "data.frame")) {
    stop("`x` must be a ggplot, gallery, or plain list.", call. = FALSE)
  }
  new_gallery(lapply(x, as_gallery_entry))
}

#' @export
`$.gallery` <- function(x, name) {
  x[[name, exact = TRUE]]
}

#' @export
`$<-.gallery` <- function(x, name, value) {
  x[[name]] <- value
  x
}

#' @export
`[[<-.gallery` <- function(x, i, ..., value) {
  if (!is.null(value)) {
    value <- as_gallery_entry(value)
  }
  new_gallery(NextMethod("[[<-"))
}

#' @export
`[<-.gallery` <- function(x, i, ..., value) {
  if (!is.null(value)) {
    value <- if (is.list(value)) lapply(value, as_gallery_entry) else as_gallery_entry(value)
  }
  new_gallery(NextMethod("[<-"))
}

#' @export
`[.gallery` <- function(x, i, ...) {
  new_gallery(NextMethod("["))
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.gallery <- function(x, pattern = "") {
  gallery_names <- names(x)
  gallery_names[!is.na(gallery_names) & startsWith(gallery_names, pattern)]
}

#' @export
print.gallery <- function(x, ...) {
  path_dir <- ggview_session()
  rstudioapi::viewer(gallery_write(x, path_dir))
  invisible(x)
}

new_gallery <- function(x) {
  if (!is.list(x)) {
    stop("A gallery must be a list.", call. = FALSE)
  }
  bad <- !vapply(x, is_gallery_entry, logical(1))
  if (any(bad)) {
    stop("Gallery entries must be ggplots or galleries.", call. = FALSE)
  }
  gallery_names <- names(x)
  if (!is.null(gallery_names)) {
    named <- !is.na(gallery_names) & nzchar(gallery_names)
    duplicates <- unique(gallery_names[named][duplicated(gallery_names[named])])
    if (length(duplicates)) {
      stop("Gallery names must be unique: ", paste(sQuote(duplicates), collapse = ", "),
           ".", call. = FALSE)
    }
  }
  structure(x, class = "gallery")
}

as_gallery_entry <- function(x) {
  if (is_gallery_entry(x)) {
    return(x)
  }
  if (is.list(x) && !inherits(x, "data.frame")) {
    return(as.gallery(x))
  }
  stop("Gallery entries must be ggplots, galleries, or plain lists.", call. = FALSE)
}

is_gallery_entry <- function(x) {
  inherits(x, "ggplot") || inherits(x, "gallery")
}

gallery_write <- function(x, path_dir, return_to = NULL, path = character()) {
  id <- basename(tempfile(pattern = "gallery_", tmpdir = path_dir))
  path_html <- file.path(path_dir, paste0(id, ".html"))
  cards <- Map(gallery_card, x, gallery_labels(x), MoreArgs = list(
    path_dir = path_dir, return_to = path_html, path = path
  ))
  writeLines(gallery_html(unlist(cards), return_to, path), path_html)
  path_html
}

gallery_card <- function(x, label, path_dir, return_to, path) {
  if (inherits(x, "gallery")) {
    path_html <- gallery_write(x, path_dir, return_to, c(path, label))
    return(paste0(
      "<a class=\"tile\" href=\"", basename(path_html), "\"><h2>",
      ggview_html_escape(label), "</h2><div class=\"thumbnail folder\"><span>&#128193;</span><p>",
      length(x), " item", if (length(x) == 1) "" else "s", "</p></div></a>"
    ))
  }
  params <- gallery_plot_params(x)
  preview <- ggview_write_preview(
    x, "png", params$scale, params$width, params$height, params$units,
    params$dpi, params$bg, path_dir, basename(tempfile(pattern = "plot_", tmpdir = path_dir)),
    return_to, c(path, label)
  )
  paste0(
    "<a class=\"tile\" href=\"", basename(preview$html), "\"><h2>",
    ggview_html_escape(label), "</h2><div class=\"thumbnail\"><img src=\"",
    basename(preview$image), "\" alt=\"", ggview_html_escape(label), "\"></div></a>"
  )
}

gallery_labels <- function(x) {
  labels <- names(x)
  if (is.null(labels)) labels <- rep("", length(x))
  unnamed <- is.na(labels) | !nzchar(labels)
  labels[unnamed] <- paste(ifelse(vapply(x[unnamed], inherits, logical(1), "gallery"),
                                  "Gallery", "Plot"), which(unnamed))
  labels
}

gallery_plot_params <- function(plot) {
  canvas <- plot$canvas
  list(
    width = canvas$width %||% 400,
    height = canvas$height %||% 300,
    units = canvas$units %||% "px",
    dpi = canvas$dpi %||% 96,
    scale = canvas$scale %||% 1,
    bg = canvas$bg %||% "white"
  )
}

gallery_html <- function(cards, return_to, path) {
  html <- readLines(system.file("gallery.html", package = "ggview"))
  html <- gsub("{{navigation}}", ggview_navigation(return_to, path), html, fixed = TRUE)
  gallery_class <- if (is.null(return_to)) "gallery" else "gallery has-back"
  html <- gsub("{{gallery_class}}", gallery_class, html, fixed = TRUE)
  html <- gsub("{{cards}}", paste(cards, collapse = ""), html, fixed = TRUE)
  paste(html, collapse = "\n")
}
