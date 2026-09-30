# Taking a whole collection out: as files, as a content table, as a gallery.

#' @title Save every plot in a collection
#' @description Writes one file per plot, each at its own canvas size. Slashes
#'   in the plot names become folders, which are created when they are missing.
#'
#' @param plots A collection, from [plots_init()].
#' @param dir Directory to write to.
#' @param device File type, and the file extension. See [ggplot2::ggsave()].
#' @param ... Passed on to [save_ggplot()].
#'
#' @return The file paths, invisibly.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "Basics/scatter", width = 5, height = 4,
#'                       show = FALSE)
#'
#' files <- save_plots(plots, dir = tempfile())
#' basename(files)
#'
#' @export
save_plots <- function(plots, dir = ".", device = "png", ...) {
  check_plots(plots)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    cli::cli_abort("{.arg dir} must be a single non-empty string.")
  }
  if (!is.character(device) || length(device) != 1L || is.na(device)) {
    cli::cli_abort("{.arg device} must be a single string, for example {.val png}.")
  }
  if (nrow(plots) == 0) {
    cli::cli_abort(c(
      "The collection is empty.",
      "i" = "Add a plot with {.fn plots_append}."
    ))
  }

  files <- file.path(dir, paste0(plots$name, ".", device))
  for (folder in unique(dirname(files))) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }

  cli::cli_progress_bar("Saving plots", total = nrow(plots))
  for (i in seq_len(nrow(plots))) {
    save_ggplot(plots_pull(plots, i), files[[i]], ...)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  if (!plots_quiet()) cli::cli_alert_success("Saved {nrow(plots)} plot{?s} to {.path {dir}}.")
  invisible(files)
}

#' @title A collection as a content table
#' @description Shapes a collection for bulk writers that take a table of
#'   objects rather than a list: one row per plot, holding the plot, the path to
#'   write it to, and the file type. The plot names become the paths, so slashes
#'   in them stay as folders.
#'
#'   The folder is the collection's own path, from [plots_init()] or
#'   [plots_set_path()], unless `path` gives another one for this write. To
#'   write only some plots, filter the collection first.
#'
#' @param plots A collection, from [plots_init()].
#' @param path Folder to write into. The collection's path when `NULL`.
#' @param extension File type, and the file extension.
#'
#' @return A tibble of `object`, `name` and `type`, one row per plot. The paths
#'   are the values of `name` and its own names.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init(path = "results/2026-09/plots")
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "Basics/scatter", show = FALSE)
#'
#' plots_as_content(plots)
#'
#' # Somewhere else for this write only, and only some plots.
#' plots_as_content(plots[plots$name == "Basics/scatter", ], path = "drafts")
#'
#' @export
plots_as_content <- function(plots, path = NULL, extension = "png") {
  check_plots(plots)
  if (nrow(plots) == 0) {
    cli::cli_abort(c(
      "The collection is empty.",
      "i" = "Add a plot with {.fn plots_append}."
    ))
  }
  path <- path %||% plots_meta_raw(plots)$path
  if (is.null(path)) {
    cli::cli_abort(c(
      "The collection has no path, and none was given.",
      "i" = "Set one with {.code plots_init(path = )} or {.fn plots_set_path}, or pass
             {.arg path} here."
    ))
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty string.")
  }
  if (!is.character(extension) || length(extension) != 1L || is.na(extension) ||
      !nzchar(extension)) {
    cli::cli_abort("{.arg extension} must be a single non-empty string.")
  }

  # Bulk writers read the path from the names of `name`, so it carries it twice.
  full_names <- file.path(path, paste0(plots$name, ".", extension))
  tibble::tibble(
    object = lapply(seq_len(nrow(plots)), function(i) plots_pull(plots, i)),
    name   = stats::setNames(full_names, full_names),
    type   = extension
  )
}

#' @export
as.gallery.plots_tbl <- function(x, ...) {
  check_plots(x)
  entries <- list()
  for (i in seq_len(nrow(x))) {
    entries <- gallery_insert(
      entries,
      strsplit(x$name[[i]], "/", fixed = TRUE)[[1]],
      plots_pull(x, i)
    )
  }
  as.gallery(entries)
}

# Put one plot into a nested list, making the folders on the way.
gallery_insert <- function(entries, path, plot) {
  taken <- entries[[path[[1]]]]
  leaf <- length(path) == 1L
  if (!is.null(taken) && inherits(taken, "ggplot") != leaf) {
    cli::cli_abort(c(
      "{.val {path[[1]]}} is the name of both a plot and a folder.",
      "i" = "Rename one of them."
    ))
  }
  if (leaf) {
    entries[[path]] <- plot
    return(entries)
  }
  entries[[path[[1]]]] <- gallery_insert(taken %||% list(), path[-1], plot)
  entries
}
