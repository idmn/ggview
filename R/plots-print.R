# How a plots collection, and what it lets people change, show themselves.

#' @title Print a plots collection
#' @description Lists the collection: every plot's name, type, canvas size and
#'   title, grouped by the folders in the names. The stored ggplots stay out of
#'   the way. In an IDE that runs links, such as RStudio, each name is a link
#'   that opens that plot. Use [plots_get()] to render one of them.
#'
#'   A plot carrying a change is marked. `tibble::as_tibble()` gives the plain
#'   tibble underneath, which prints the way any tibble does.
#'
#' @param x A collection, from [plots_init()].
#' @param n Number of plots to list. The default lists all of them.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init(name = "Basics")
#' plots <- plots_append(plots, p + labs(title = "Weight vs mileage"),
#'                       name = "Basics/scatter", type = "point", show = FALSE)
#' plots <- plots_append(plots, p + geom_smooth(),
#'                       name = "Basics/trend", type = "line", show = FALSE)
#' print(plots)
#'
#' @export
print.plots_tbl <- function(x, n = Inf, ...) {
  meta <- plots_meta_raw(x)
  folders <- plots_folder(x$name)
  n_folders <- length(unique(folders[nzchar(folders)]))
  label <- if (is.null(meta$name)) "{.cls plots_tbl}" else
    paste0("{.cls plots_tbl} \u00b7 ", cli_escape(meta$name))
  cli::cli_rule(
    left = label,
    right = paste0("{nrow(x)} plot{?s}", if (n_folders) " in {n_folders} folder{?s}")
  )
  if (!is.null(meta$path)) {
    cli::cli_text(cli::col_grey("Goes to {.path {meta$path}}"))
  }

  if (nrow(x) == 0) {
    cli::cli_text(cli::col_grey("Empty \u2014 add a plot with {.fn plots_append}."))
    return(invisible(x))
  }

  # The links in the listing render from here, because a link carries only a
  # literal and cannot name the variable the collection is held in.
  the$printed <- x

  shown <- seq_len(min(nrow(x), n))
  cli::cli_verbatim(plots_lines(x[shown, ], folders[shown]))

  if (length(shown) < nrow(x)) {
    cli::cli_text(cli::col_grey(
      "\u2026 and {nrow(x) - length(shown)} more. Use {.code print(plots, n = Inf)}."
    ))
  }
  untitled <- sum(is.na(plots_titles(x)))
  if (untitled) {
    cli::cli_text(cli::col_grey("{untitled} plot{?s} without a title."))
  }
  changed <- sum(lengths(x$values) > 0)
  if (changed) {
    cli::cli_text(cli::col_grey(
      "{changed} plot{?s} changed \u00b7 {.code plots_reset()} forgets {?it/them}"
    ))
  }
  if (plots_links_work()) {
    cli::cli_text(cli::col_grey(
      "Click a name to view it \u00b7 {.code as.gallery()} browses them all"
    ))
  } else {
    cli::cli_text(cli::col_grey(
      "{.code plots_get()} renders one \u00b7 {.code as.gallery()} browses them all"
    ))
  }
  invisible(x)
}

# One line per plot, with a heading wherever the folder changes. Cells are
# padded as plain text and colored afterwards, so the columns stay aligned.
plots_lines <- function(x, folders) {
  leaf <- plots_leaf(x$name)
  type <- ifelse(is.na(x$type), "", x$type)
  canvas <- lapply(seq_len(nrow(x)), function(i) plots_size(x, i))
  times <- if (cli::is_utf8_output()) "\u00d7" else "x"
  size <- paste(fmt_number(vapply(canvas, function(s) s$width, numeric(1))), times,
                fmt_number(vapply(canvas, function(s) s$height, numeric(1))))
  index <- format(seq_len(nrow(x)), width = max(2, nchar(nrow(x))))
  mark <- ifelse(lengths(x$values) > 0, "*", " ")
  titles <- plots_titles(x)

  width_leaf <- min(max(nchar(leaf)), 40)
  width_type <- max(nchar(type))
  width_size <- max(nchar(size))
  room <- cli::console_width() -
    (nchar(index[[1]]) + width_leaf + width_type + width_size + 10)

  out <- character()
  for (i in seq_len(nrow(x))) {
    if (i == 1 || folders[[i]] != folders[[i - 1]]) {
      if (nzchar(folders[[i]])) {
        out <- c(out, cli::style_bold(cli::col_blue(folders[[i]])))
      }
    }
    title <- if (is.na(titles[[i]])) {
      cli::col_silver(shorten("<no title>", room))
    } else {
      cli::col_grey(shorten(one_line(titles[[i]]), room))
    }
    out <- c(out, paste0(
      cli::col_silver(index[[i]]), cli::col_yellow(mark[[i]]), " ",
      plots_link(shorten(leaf[[i]], width_leaf), x$name[[i]], width_leaf), " ",
      cli::col_blue(pad(type[[i]], width_type)), " ",
      cli::col_silver(pad(size[[i]], width_size)), " ",
      title
    ))
  }
  out
}

# The title a plot shows: the change, or the plot's own, or none.
plots_titles <- function(x) {
  vapply(seq_len(nrow(x)), function(i) {
    value <- x$values[[i]]$title
    if (is.null(value)) return(plot_label(x$plot[[i]], "title"))
    if (is.na(value)) NA_character_ else as.character(value)
  }, character(1))
}

#' @title Print what a plot lets people change
#' @description Lists the parameters from [plots_params()]: what each one is
#'   called, what it takes, what it is by default, and what it was changed to.
#'
#' @param x Parameters, from [plots_params()].
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", title = "Weight vs mileage", show = FALSE)
#' plots_params(plots, "scatter")
#'
#' @export
print.plots_params <- function(x, ...) {
  cli::cli_rule(left = "{.cls plots_params}", right = "{length(x)} parameter{?s}")

  keys <- vapply(x, function(p) p$key, character(1))
  types <- vapply(x, function(p) p$type, character(1))
  defaults <- vapply(x, function(p) param_text_of(p[["default"]]), character(1))
  values <- vapply(x, function(p) param_text_of(p[["value"]]), character(1))

  width_key <- max(nchar(keys))
  width_type <- max(nchar(types))
  arrow <- if (cli::is_utf8_output()) "\u2192" else "->"

  lines <- character(length(x))
  for (i in seq_along(x)) {
    changed <- !is.null(x[[i]][["value"]])
    allowed <- param_allowed(x[[i]])
    lines[[i]] <- paste0(
      "  ", pad(keys[[i]], width_key), " ",
      cli::col_blue(pad(types[[i]], width_type)), " ",
      if (changed) cli::col_silver(defaults[[i]]) else defaults[[i]],
      if (changed) paste0(" ", arrow, " ", cli::col_yellow(values[[i]])) else "",
      if (nzchar(allowed)) cli::col_silver(paste0("  ", allowed)) else ""
    )
  }
  cli::cli_verbatim(shorten(lines, cli::console_width()))
  invisible(x)
}

# What a parameter will accept, when it accepts only some things.
param_allowed <- function(param) {
  if (!is.null(param$choices)) return(paste0("(", paste(param$choices, collapse = ", "), ")"))
  if (!is.null(param$keys)) {
    return(paste0("(", param$to, " per ", paste(param$keys, collapse = ", "), ")"))
  }
  ""
}

# How a default or a value reads in the listing.
param_text_of <- function(value) {
  if (is.null(value)) return("")
  if (length(value) == 1L && !is.list(value) && is.na(value)) return("<none>")
  if (is.list(value) || length(value) > 1L) {
    value <- unlist(value)
    text <- if (!is.null(names(value))) {
      paste0(names(value), "=", value, collapse = ", ")
    } else {
      paste(value, collapse = ", ")
    }
    return(one_line(text))
  }
  one_line(as.character(value))
}

# Text on one line: a line break shows as a mark rather than breaking the listing.
one_line <- function(x) {
  gsub("\n", if (cli::is_utf8_output()) "\u21b5" else "\\n", x, fixed = TRUE)
}

#' @title Print default canvas sizes
#' @description Lists the canvas size each plot type gets by default.
#'
#' @param x Sizes, from [plots_dims()].
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' plots_dims(heatmap = c(18, 11))
#'
#' @export
print.plots_dims <- function(x, ...) {
  cli::cli_rule(left = "{.cls plots_dims}", right = "inches")
  types <- names(x)
  sizes <- vapply(
    x,
    function(size) {
      paste(fmt_number(size[[1]]), if (cli::is_utf8_output()) "\u00d7" else "x",
            fmt_number(size[[2]]))
    },
    character(1)
  )
  cli::cli_verbatim(paste0(
    "  ", pad(types, max(nchar(types))), "  ", cli::col_silver(sizes)
  ))
  invisible(x)
}

# --- formatting helpers ------------------------------------------------------

# Folder part of a name ("a/b/c" -> "a/b"), empty when the name has no folder.
plots_folder <- function(name) {
  ifelse(grepl("/", name, fixed = TRUE), sub("/[^/]*$", "", name), "")
}

# Last part of a name ("a/b/c" -> "c").
plots_leaf <- function(name) {
  sub("^.*/", "", name)
}

# A plot name that opens the plot when it is clicked, padded to `width`. The
# link runs `plots_show()`, which renders from the collection this print just
# stored: a link carries a literal, never the variable holding the collection.
# The IDE echoes whatever the link runs, so it runs the plot's name and not its
# id, and `plots_show()` itself says nothing.
plots_link <- function(text, name, width) {
  padding <- strrep(" ", max(0, width - nchar(text)))
  if (!plots_links_work()) return(paste0(text, padding))
  code <- paste0("ggview::plots_show(", encodeString(name, quote = "\""), ")")
  paste0(cli::format_inline("{.run [{text}]({code})}"), padding)
}

plots_links_work <- function() {
  isTRUE(cli::ansi_hyperlink_types()$run)
}

# Shorten anything longer than `width`.
shorten <- function(x, width) {
  cli::ansi_strtrim(x, max(width, 3))
}

# Shorten, then pad to `width`.
pad <- function(x, width) {
  formatC(shorten(x, width), width = -width, flag = " ")
}

# 15 rather than 15.0, but 7.5 stays 7.5.
fmt_number <- function(x) {
  format(round(x, 2), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
}

# Text that cli must not read as an expression.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
