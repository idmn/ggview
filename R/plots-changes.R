# What somebody changed about a collection, as a table and as the code that
# makes the same changes again.

#' @title What a collection's changes are
#' @description Lists every change recorded against the plots, one row per
#'   parameter: what it is set to, and what it would be without the change. A
#'   change equal to its default is left out, and so is each key of a mapping
#'   that still holds its default.
#'
#'   Use it to see what somebody changed in a stored collection, for example in
#'   a viewer that saves its changes back. [plots_changes_code()] turns the same
#'   changes into code for the script that builds the collection.
#'
#' @param plots A collection, from [plots_init()].
#'
#' @return A tibble with one row per change: `name` of the plot, `key` of the
#'   parameter, and `value` and `default` as list columns.
#'
#' @seealso [plots_changes_code()], [plots_params()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", show = FALSE)
#' plots <- plots_set(plots, "scatter", title = "Heavier cars use more fuel",
#'                    width = 8, show = FALSE)
#'
#' plots_changes(plots)
#'
#' @export
plots_changes <- function(plots) {
  check_plots(plots)
  rows <- lapply(seq_len(nrow(plots)), function(i) {
    values <- plots$values[[i]]
    if (!length(values)) return(NULL)
    params <- plots_params(plots, id = plots$id[[i]])
    changes <- lapply(names(values), function(key) {
      default <- params[[key]]$default
      value <- changed_part(values[[key]], default, params[[key]]$type)
      if (is.null(value)) return(NULL)
      tibble::tibble(name = plots$name[[i]], key = key, value = list(value),
                     default = list(default))
    })
    do.call(rbind, changes)
  })
  out <- do.call(rbind, rows)
  out %||% tibble::tibble(name = character(), key = character(), value = list(),
                          default = list())
}

#' @title The code that makes a collection's changes again
#' @description Writes the changes recorded against a collection as a pipe:
#'   [plots_reset()] first, then its path as a [plots_set_path()] call, then one
#'   [plots_set()] call per changed plot. Run the code on the collection a
#'   script builds, after the plots are added, and the rebuilt collection holds
#'   exactly the same changes: a change the script makes that the stored
#'   collection took away is gone again too.
#'
#'   This is how changes made outside the script, for example in a viewer that
#'   saves a stored collection back, go into the script: the code replaces the
#'   script's own changes, and the script stays the one place that says what the
#'   collection is. Plots are named by `name`, not by `id`, because a rebuilt
#'   collection gets new ids. Changes for plots the script added since belong
#'   after the pipe, because the reset takes them away too.
#'
#' @inheritParams plots_changes
#' @param object Name of the variable that holds the collection in the script.
#'
#' @return The code, as a single string of class `plots_code`. Printing it
#'   shows the code.
#'
#' @seealso [plots_changes()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init(path = "results/2026-09/plots")
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "Basics/scatter", show = FALSE)
#' plots <- plots_set(plots, "Basics/scatter", title = "Heavier cars use more fuel",
#'                    width = 8, show = FALSE)
#'
#' plots_changes_code(plots)
#'
#' @export
plots_changes_code <- function(plots, object = "plots") {
  check_plots(plots)
  if (!is.character(object) || length(object) != 1L || is.na(object) ||
      make.names(object) != object) {
    cli::cli_abort("{.arg object} must be the name of a variable, such as {.val plots}.")
  }
  changes <- plots_changes(plots)
  path <- plots_meta_raw(plots)$path

  if (nrow(plots) == 0) {
    return(structure("# The collection holds no plots.", class = "plots_code"))
  }
  calls <- "plots_reset(show = FALSE)"
  if (!is.null(path)) calls <- c(calls, paste0("plots_set_path(", code_value(path), ")"))
  for (name in unique(changes$name)) {
    mine <- changes[changes$name == name, ]
    args <- c(code_value(name),
              paste0(code_key(mine$key), " = ", vapply(mine$value, code_value, character(1))),
              "show = FALSE")
    calls <- c(calls, code_call("plots_set", args))
  }

  code <- paste0(object, " <- ", object, " |>\n", paste0("  ", calls, collapse = " |>\n"))
  structure(code, class = "plots_code")
}

#' @export
print.plots_code <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}

# The part of a value that differs from its default: NULL when none does. A
# mapping keeps only the keys that changed, because [plots_set()] fills the rest
# from the default.
changed_part <- function(value, default, type) {
  if (identical(type, "mapping") && is.list(value) && length(default)) {
    default <- as.list(default)
    keep <- vapply(names(value), function(k) !same_value(value[[k]], default[[k]]),
                   logical(1))
    return(if (any(keep)) value[keep] else NULL)
  }
  if (same_value(value, default)) NULL else value
}

# Equal as a plot sees them: `NA` for a label is the same as no label, whatever
# type the `NA` is, and 12 is the same as 12L.
same_value <- function(a, b) {
  if (identical(a, b)) return(TRUE)
  if (!is.atomic(a) || !is.atomic(b) || length(a) != length(b)) return(FALSE)
  a <- unname(a)
  b <- unname(b)
  if (!identical(is.na(a), is.na(b))) return(FALSE)
  if (all(is.na(a))) return(TRUE)
  is.numeric(a) && is.numeric(b) && isTRUE(all.equal(a[!is.na(a)], b[!is.na(b)]))
}

# One call on one line when it fits in 100 characters, otherwise one argument
# per line.
code_call <- function(fun, args) {
  one <- paste0(fun, "(", paste(args, collapse = ", "), ")")
  if (nchar(one) <= 96L) return(one)
  paste0(fun, "(\n", paste0("    ", args, collapse = ",\n"), "\n  )")
}

# A value as code. A whole number that came in as an integer, from JSON for example, is written
# as the number it is: 10, not 10L.
code_value <- function(x) {
  if (is.list(x)) x[] <- lapply(x, function(v) if (is.integer(v)) as.double(v) else v)
  if (is.integer(x)) x <- as.double(x)
  deparse1(x, collapse = " ", width.cutoff = 500L)
}

# A key that is not a syntactic name is written as a string, which R takes as an
# argument name as it is: backticks would break on a backtick or a backslash in it.
code_key <- function(key) {
  ifelse(make.names(key) == key, key, vapply(key, code_value, character(1), USE.NAMES = FALSE))
}
