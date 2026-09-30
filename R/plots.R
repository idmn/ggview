# A plots collection: one row per plot, its canvas as columns, and whatever
# somebody changed about it kept apart from the plot itself.

#' @title Start a plots collection
#' @description A plots collection keeps a set of ggplots together with the
#'   canvas each one is drawn at and the changes somebody has made to it. It is
#'   a tibble with one row per plot, so `dplyr` verbs work on it.
#'
#'   A change is never written into the plot. [plots_get()] takes the stored
#'   plot, applies the changes, then applies the canvas, so the same plot can be
#'   changed again and again, and [plots_reset()] always has something to go
#'   back to.
#'
#'   Collect plots with [plots_append()], take one out with [plots_get()],
#'   change one with [plots_set()], and write them all with [save_plots()].
#'
#'   `path` says where the collection's files go, as a folder relative to the
#'   root of whatever writes them. [plots_as_content()] uses it, so the
#'   collection itself records where it was delivered. [plots_set_path()]
#'   changes it later.
#'
#'   The collection keeps each customizer once, under a name, and each plot
#'   names the one it uses. The functions in `functions` travel with the
#'   collection, so every customizer can call them by name instead of repeating
#'   them. Themes are kept once too: plots that share a theme store one copy,
#'   and [plots_get()] puts it back.
#'
#'   A collection is read back in other sessions, so what it carries must not
#'   depend on this one. `plots_init()` refuses a customizer or function that
#'   uses a name from the session, or an unqualified function from an attached
#'   package other than base R, ggplot2, forcats and ggview: write
#'   `stringr::str_wrap()`, not `str_wrap()`. A reader attaches those four.
#'
#' @section Options:
#'   `options(ggview.quiet = TRUE)` silences the one-line reports of
#'   [plots_append()], [plots_get()], [plots_reset()] and [save_plots()], for
#'   loops over many plots. Warnings still show.
#'
#' @param name A name for the collection, for whatever shows it to people.
#' @param description One line saying what the collection is.
#' @param path Folder the collection's files go to, for example
#'   `"results/2026-09/plots/US"`. It is relative, with a slash between
#'   folders and none at either end. `NULL` for none yet.
#' @param dims Default canvas size per plot type, from [plots_dims()].
#' @param customizers What plots in this collection let people change: a named
#'   list of [customizer()]s. A plot names one in [plots_append()], and
#'   `default` is the one it gets when it names none. `default` is
#'   [customizer_default()] unless you give your own.
#' @param functions Functions every customizer can call, as a named list. They
#'   can call each other too. Use them for what several customizers share.
#'
#' @return An empty collection of class `plots_tbl`, with columns `id`, `name`,
#'   `type`, `width`, `height`, `plot`, `customizer`, `theme` and `values`.
#'
#' @seealso [plots_append()], [plots_get()], [plots_set()], [plots_params()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init(name = "Brand tracker")
#' plots <- plots_append(
#'   plots,
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(title = "Weight vs mileage"),
#'   name = "Basics/scatter", type = "point", show = FALSE
#' )
#' plots
#'
#' # Wider heatmaps than the default, for every plot in this collection.
#' plots_init(dims = plots_dims(heatmap = c(18, 11)))
#'
#' # Where the files go, for plots_as_content().
#' plots_init(name = "Brand tracker", path = "results/2026-09/plots")
#'
#' # One shared function, used by a customizer that plots name.
#' shades <- customizer_extend(
#'   customizer_default(),
#'   apply = function(plot, fill = NULL) {
#'     if (is.null(fill)) plot else recolor(plot, fill)
#'   },
#'   params = function(plot) list(fill = param_color("Bar color"))
#' )
#' plots <- plots_init(
#'   customizers = list(shades = shades),
#'   functions = list(
#'     recolor = function(plot, fill) plot + geom_bar(fill = fill)
#'   )
#' )
#' plots <- plots_append(plots, ggplot(mtcars, aes(factor(gear))) + geom_bar(),
#'                       name = "gears", customizer = "shades", fill = "#36c8ef",
#'                       show = FALSE)
#'
#' @export
plots_init <- function(name = NULL, description = NULL, path = NULL,
                       dims = plots_dims(), customizers = list(), functions = list()) {
  check_line(name, "name")
  check_line(description, "description")
  check_path(path)
  check_dims(dims)
  functions <- check_functions(functions)
  customizers <- check_customizers(customizers, known = names(functions))
  if (is.null(customizers$default)) {
    customizers <- c(list(default = customizer_default()), customizers)
  }

  new_plots_tbl(
    tibble::tibble(
      id         = character(),
      name       = character(),
      type       = character(),
      width      = double(),
      height     = double(),
      plot       = list(),
      customizer = character(),
      theme      = character(),
      values     = list()
    ),
    meta = list(
      name        = name,
      description = description,
      path        = path,
      dims        = dims,
      customizers = customizers,
      functions   = functions,
      themes      = list(),
      packages    = character()
    )
  )
}

#' @title Add a plot to a collection
#' @description Adds one plot to the end of a collection. The plot is stored as
#'   it is: anything given in `...` is kept beside it as a change, not written
#'   into it. Adding a name that is already in the collection updates that row
#'   in place and keeps its `id` and the changes recorded against it, so
#'   re-running a chunk is safe. A change the new plot no longer accepts is
#'   dropped, and `...` replaces a change of the same name.
#'
#'   The plot's theme goes to the collection's store, which keeps one copy of
#'   each distinct theme. A plot whose scales, layers or facets carry a function
#'   that uses a name from this session, or an unqualified function from an
#'   attached package, draws here and fails elsewhere, so it is added with a
#'   warning that names it.
#'
#' @param plots A collection, from [plots_init()].
#' @param plot A ggplot object.
#' @param name Name of the plot. It must be unique in the collection. Slashes
#'   make folders when the collection is saved or browsed, for example
#'   `"Module 1/awareness"`.
#' @param type Plot type, for example `"bar"` or `"heatmap"`. It selects the
#'   default canvas size and groups the collection for bulk re-sizing.
#' @param width,height Canvas size in inches. Each falls back to a [canvas()]
#'   already on the plot, then to the collection's default for `type`.
#' @param customizer What this plot lets people change: the name of one of the
#'   collection's customizers, given to [plots_init()].
#' @param ... Changes to store with the plot, named after the parameters its
#'   customizer offers — `title = "…"` and so on. [plots_params()] lists them.
#' @param show Whether to preview the plot. It is shown at its true output size
#'   in the RStudio viewer, and drawn the ordinary way anywhere else.
#'
#' @return The collection, with the plot added.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl))) + geom_bar() + labs(title = "Cylinders")
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "Engine/cylinders", type = "bar", show = FALSE)
#'
#' # The plot keeps its own title; the collection keeps the new one.
#' plots <- plots_append(plots, p, name = "Engine/cylinders", type = "bar",
#'                       title = "Most of these are eight-cylinder", show = FALSE)
#' plots_get(plots, "Engine/cylinders")$labels$title
#'
#' @export
plots_append <- function(plots, plot, name, type = NA_character_,
                         width = NULL, height = NULL, customizer = "default",
                         ..., show = interactive()) {
  check_plots(plots)
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a {.cls ggplot} object, not {.cls {class(plot)[[1]]}}.")
  }
  check_name(name)
  check_type(type)

  meta <- plots_meta_raw(plots)
  if (!is.character(customizer) || length(customizer) != 1L ||
      is.null(meta$customizers[[customizer]])) {
    cli::cli_abort(c(
      "{.arg customizer} must name one of the collection's customizers.",
      "i" = "It has {.val {names(meta$customizers)}}. Give others to {.fn plots_init}."
    ))
  }
  row_customizer <- customizer
  customizer <- bind_functions(meta$customizers[[customizer]], meta$functions)

  size <- plots_dims_lookup(meta$dims, type)
  on_plot <- canvas_inches(plot$canvas)
  width <- width %||% on_plot$width %||% size$width
  height <- height %||% on_plot$height %||% size$height

  values <- check_values(list(...), customizer, plot)
  warn_plot_travel(plot, name)

  # One copy of each distinct theme, in the collection. The packages that draw
  # its elements are recorded, because reading a collection does not load them.
  theme_key <- rlang::hash(plot$theme)
  if (is.null(meta$themes[[theme_key]])) {
    meta$themes[[theme_key]] <- plot$theme
    meta$packages <- union(meta$packages, theme_packages(plot$theme))
  }
  stored <- plot
  stored$theme <- ggplot2::theme()

  row <- tibble::tibble(
    id         = plots_new_id(plots$id),
    name       = name,
    type       = as.character(type),
    width      = check_size(width, "width"),
    height     = check_size(height, "height"),
    plot       = list(stored),
    customizer = row_customizer,
    theme      = theme_key,
    values     = list(values)
  )

  i <- match(name, plots$name)
  if (is.na(i)) {
    out <- rbind(tibble::as_tibble(plots), row)
    verb <- "Added"
  } else {
    row$id <- plots$id[[i]]
    kept <- values_still_valid(plots$values[[i]], customizer, plot, row$width, row$height)
    kept <- kept[setdiff(names(kept), names(values))]
    row$values <- list(c(kept, values))
    out <- plots
    out[i, ] <- row
    verb <- "Updated"
  }
  out <- new_plots_tbl(out, meta = keep_used_themes(meta, out))

  n <- nrow(out)
  if (!plots_quiet()) {
    cli::cli_alert_success(
      "{verb} {.val {name}} \u2014 {plots_describe(row$type, row$width, row$height)}. {n} plot{?s} in the collection."
    )
  }

  if (isTRUE(show)) print(plots_pull(out, match(name, out$name)))
  out
}

#' @title Take a plot out of a collection
#' @description Returns one plot, ready to render: the stored plot with its
#'   changes applied and its canvas set. Printing the result shows it at the
#'   size it will be saved at.
#'
#'   The changes are applied to the stored plot every time, never to an
#'   already-changed one, so asking twice gives the same plot twice.
#'
#' @param plots A collection, from [plots_init()].
#' @param name Name of the plot. The last plot in the collection is returned
#'   when both `name` and `id` are omitted.
#' @param id Id of the plot, as an alternative to `name`. An id stays the same
#'   when a plot is updated, so it is the safer key for code that edits a
#'   stored collection.
#'
#' @return A ggplot object with a [canvas()].
#'
#' @seealso [save_plots()] and [as.gallery()] for every plot at once.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point", show = FALSE)
#'
#' p <- plots_get(plots, "scatter")
#' p$canvas$width
#'
#' @export
plots_get <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  i <- plots_locate(plots, name, id, .last = TRUE)
  size <- plots_size(plots, i)
  if (!plots_quiet()) {
    cli::cli_alert_info(
      "{.val {plots$name[[i]]}} \u2014 {plots_describe(plots$type[[i]], size$width, size$height)}."
    )
  }
  plots_pull(plots, i)
}

# One plot by row number, without the message. Everything that takes several
# plots at once goes through here.
plots_pull <- function(plots, i) {
  plot <- plots_plot(plots, i)
  values <- plots$values[[i]]
  changes <- values[setdiff(names(values), plots_reserved)]

  if (length(changes)) {
    customizer <- plots_customizer(plots, i)
    plot <- tryCatch(
      customizer_apply(customizer, plot, changes),
      error = function(e) {
        cli::cli_abort(
          c("The customizer for {.val {plots$name[[i]]}} failed.",
            "i" = "Changes applied: {.val {names(changes)}}."),
          parent = e, call = NULL
        )
      }
    )
    if (!inherits(plot, "ggplot")) {
      cli::cli_abort(c(
        "The customizer for {.val {plots$name[[i]]}} did not return a plot.",
        "x" = "It returned {.cls {class(plot)[[1]]}}."
      ))
    }
  }
  # Checked here rather than only where it is set, because these are ordinary
  # columns and a `mutate()` can put anything in them.
  size <- plots_size(plots, i)
  for (side in c("width", "height")) {
    value <- size[[side]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      cli::cli_abort(c(
        "The canvas for {.val {plots$name[[i]]}} is not usable.",
        "x" = "Its {.field {side}} is {.val {value}}.",
        "i" = "Set one with {.fn plots_set}, or go back with {.fn plots_reset}."
      ))
    }
  }
  # The collection owns the size. Resolution, scale and background stay as the
  # plot's own canvas set them.
  own <- plot$canvas
  plot + canvas(width = size$width, height = size$height,
                dpi = own$dpi %||% 300, scale = own$scale %||% 1, bg = own$bg %||% "white")
}

# "bar, 15 x 9 in", for the messages.
plots_describe <- function(type, width, height) {
  size <- paste0(fmt_number(width), " \u00d7 ", fmt_number(height), " in")
  if (is.na(type)) size else paste0(type, ", ", size)
}

#' @title Change a plot in a collection
#' @description Records a change against one plot. The stored ggplot is not
#'   touched, so a change is always reversible with [plots_reset()] and never
#'   re-runs the code that built the plot.
#'
#'   Which changes a plot accepts depends on its customizer, and
#'   [plots_params()] lists them. `title`, `subtitle` and `caption` are named
#'   here because most customizers offer them; anything else is passed by name
#'   through `...`.
#'
#' @inheritParams plots_get
#' @param ... Changes named after the parameters the plot's customizer offers.
#'   `NA` takes a value away, for example `title = NA` for no title.
#' @param title,subtitle,caption Label text, for customizers that offer them.
#' @param width,height Canvas size in inches. These belong to the collection
#'   rather than the customizer, so every plot accepts them. The size the plot was
#'   added with is what [plots_reset()] goes back to.
#' @param show Whether to preview the changed plot. It is shown at its true
#'   output size in the RStudio viewer, and drawn the ordinary way anywhere
#'   else. In a pipe of several calls each one previews, so pass `FALSE` when
#'   tuning several plots at once.
#'
#' @return The collection, with that change recorded.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point", show = FALSE)
#'
#' plots <- plots_set(plots, "scatter",
#'                    title = "Heavier cars use more fuel",
#'                    caption = "Source: mtcars", width = 8, height = 5)
#' plots
#'
#' # Take a label away again, and then forget the change entirely.
#' plots <- plots_set(plots, "scatter", caption = NA)
#' plots <- plots_reset(plots, "scatter", params = "caption")
#'
#' @export
plots_set <- function(plots, name = NULL, id = NULL, ...,
                      title = NULL, subtitle = NULL, caption = NULL,
                      width = NULL, height = NULL, show = interactive()) {
  check_plots(plots)
  i <- plots_locate(plots, name, id)

  named <- list(title = title, subtitle = subtitle, caption = caption,
                width = width, height = height)
  changes <- c(named[!vapply(named, is.null, logical(1))], list(...))
  changes <- check_values(
    changes, plots_customizer(plots, i), plots_plot(plots, i),
    geometry = geometry_params(plots$width[[i]], plots$height[[i]]),
    existing = plots$values[[i]]
  )

  if (length(changes)) {
    values <- plots$values[[i]]
    values[names(changes)] <- changes
    plots$values[[i]] <- values
  }

  if (isTRUE(show)) print(plots_pull(plots, i))
  plots
}

#' @title Forget changes made to a plot
#' @description Drops the changes recorded against a plot. The plot itself was
#'   never changed and neither was the canvas it was added with, so what comes
#'   back is the plot as the script declared it.
#'
#' @inheritParams plots_get
#' @param name,id Which plots to reset. Every plot in the collection when both
#'   are omitted.
#' @param params Which changes to forget, by name, for example
#'   `c("title", "width")`. All of them when `NULL`.
#' @param show Whether to preview the plot afterwards. Only when resetting one.
#'
#' @return The collection, with those changes forgotten.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", title = "Changed", show = FALSE)
#'
#' plots_get(plots, "scatter")$labels$title
#' plots <- plots_reset(plots)
#' plots_get(plots, "scatter")$labels$title
#'
#' @export
plots_reset <- function(plots, name = NULL, id = NULL, params = NULL,
                        show = interactive()) {
  check_plots(plots)
  rows <- if (is.null(name) && is.null(id)) {
    seq_len(nrow(plots))
  } else {
    plots_locate_all(plots, name, id)
  }
  if (!is.null(params) && (!is.character(params) || anyNA(params))) {
    cli::cli_abort("{.arg params} must be a character vector without {.val NA}.")
  }
  if (!is.null(params)) {
    held <- unique(unlist(lapply(rows, function(i) names(plots$values[[i]]))))
    unknown <- setdiff(params, held)
    if (length(unknown)) {
      cli::cli_abort(c(
        "Nothing to reset for {.val {unknown}}.",
        "i" = if (length(held)) "These plots hold changes to {.val {held}}."
              else "These plots hold no changes at all."
      ))
    }
  }

  dropped <- 0L
  for (i in rows) {
    values <- plots$values[[i]]
    keep <- if (is.null(params)) character() else setdiff(names(values), params)
    dropped <- dropped + length(values) - length(keep)
    plots$values[[i]] <- if (length(keep)) values[keep] else list()
  }
  if (!plots_quiet()) {
    cli::cli_alert_success("Reset {dropped} change{?s} on {length(rows)} plot{?s}.")
  }

  if (isTRUE(show) && length(rows) == 1L) print(plots_pull(plots, rows))
  plots
}

#' @title Remove plots from a collection
#' @description Drops one or more plots. Everything else keeps its order and
#'   its id.
#'
#' @param plots A collection, from [plots_init()].
#' @param name Names of the plots to remove.
#' @param id Ids of the plots to remove, as an alternative to `name`.
#'
#' @return The collection, without those plots.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "keep", show = FALSE)
#' plots <- plots_append(plots, p, name = "drop", show = FALSE)
#'
#' plots_remove(plots, "drop")
#'
#' @export
plots_remove <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  plots[-plots_locate_all(plots, name, id), ]
}

#' @title What a plot lets people change
#' @description Lists one plot's parameters: what each one is called, what kind
#'   of value it takes, what it is by default, and what it has been changed to.
#'   This is what a program reads to offer the right control for each one, and
#'   what [plots_set()] checks a change against.
#'
#'   The canvas comes last. Every plot accepts `width` and `height` whatever
#'   its customizer offers.
#'
#' @inheritParams plots_get
#'
#' @return A list with one entry per parameter, each holding `key`, `type`,
#'   `label`, `default`, and `value` when the parameter has been changed. A
#'   `choice` also carries its `choices`, a `mapping` its `keys` and what it
#'   maps `to`, and a `number` any `min`, `max` and `step`.
#'
#' @seealso [customizer()], [plots_set()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point",
#'                       title = "Weight vs mileage", show = FALSE)
#'
#' plots_params(plots, "scatter")
#'
#' @export
plots_params <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  i <- plots_locate(plots, name, id, .last = TRUE)

  params <- customizer_params(plots_customizer(plots, i), plots_plot(plots, i))
  values <- plots$values[[i]]
  records <- lapply(names(params), function(key) {
    param_record(key, params[[key]], values[[key]])
  })

  geometry <- geometry_params(plots$width[[i]], plots$height[[i]])
  records <- c(records, list(
    param_record("width", geometry$width, values[["width"]]),
    param_record("height", geometry$height, values[["height"]])
  ))
  structure(records, names = vapply(records, function(r) r$key, character(1)),
            class = "plots_params")
}

param_record <- function(key, param, value) {
  record <- c(list(key = key), unclass(param))
  record$value <- value
  record[!vapply(record, is.null, logical(1))]
}

#' @title What a collection is
#' @description The collection's own details, rather than any one plot's: the
#'   name, description and path given to [plots_init()], how many plots it
#'   holds, the names of its customizers and functions, how many distinct
#'   themes it stores, and the packages its plots need to draw.
#'
#' @param plots A collection, from [plots_init()].
#'
#' @return A list of `name`, `description`, `path`, `plots`, `customizers`,
#'   `functions`, `themes` and `packages`.
#'
#' @examples
#' plots_meta(plots_init(name = "Brand tracker", description = "Q3 wave"))
#'
#' @export
plots_meta <- function(plots) {
  check_plots(plots)
  meta <- plots_meta_raw(plots)
  list(
    name        = meta$name,
    description = meta$description,
    path        = meta$path,
    plots       = nrow(plots),
    customizers = names(meta$customizers),
    functions   = names(meta$functions),
    themes      = length(meta$themes),
    packages    = meta$packages
  )
}

#' @title Change where a collection's files go
#' @description Sets the folder [plots_as_content()] writes the collection to.
#'   The plots and their changes stay as they are. Files already written to
#'   the old folder stay there too: the collection only records where it goes
#'   next.
#'
#' @param plots A collection, from [plots_init()].
#' @param path Folder the files go to, relative, with a slash between folders
#'   and none at either end. `NULL` takes the path away.
#'
#' @return The collection, with the new path.
#'
#' @examples
#' plots <- plots_init(path = "results/2026-06/plots")
#' plots <- plots_set_path(plots, "results/2026-09/plots")
#' plots_meta(plots)$path
#'
#' @export
plots_set_path <- function(plots, path) {
  check_plots(plots)
  check_path(path)
  meta <- plots_meta_raw(plots)
  meta["path"] <- list(path)
  new_plots_tbl(plots, meta = meta)
}

#' @title Show a plot from the collection that was printed last
#' @description Opens one plot in the viewer at its canvas size, and says
#'   nothing else. Printing a collection makes every plot name a link to this
#'   function, so clicking a name opens that plot. The links need an IDE that
#'   runs them, such as RStudio.
#'
#' @param id Name of the plot, or its id.
#'
#' @return The plot, invisibly.
#'
#' @seealso [plots_get()], which takes a plot from a collection you name.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", show = FALSE)
#' print(plots)
#'
#' if (rstudioapi::isAvailable()) plots_show("scatter")
#'
#' @export
plots_show <- function(id) {
  plots <- the$printed
  if (is.null(plots)) {
    cli::cli_abort(c(
      "No collection has been printed yet.",
      "i" = "Print one, then click a plot name in it."
    ))
  }
  i <- match(id, plots$id)
  if (is.na(i)) i <- match(id, plots$name)
  if (is.na(i)) {
    cli::cli_abort(c(
      "{.val {id}} is not in the collection that was printed last.",
      "i" = "Print the collection again, then click a plot name in it."
    ))
  }
  invisible(print(plots_pull(plots, i)))
}

# --- the collection object ---------------------------------------------------

# The collection printed last, so the links in a printed collection have
# something to render from. A link can only carry a literal.
the <- new.env(parent = emptyenv())
the$printed <- NULL

new_plots_tbl <- function(x, meta = NULL) {
  attr(x, "meta") <- meta %||% attr(x, "meta") %||% plots_meta_default()
  class(x) <- unique(c("plots_tbl", class(x)))
  x
}

plots_meta_default <- function() {
  list(
    name = NULL, description = NULL, path = NULL, dims = plots_dims(),
    customizers = list(default = customizer_default()), functions = list(),
    themes = list(), packages = character()
  )
}

plots_quiet <- function() isTRUE(getOption("ggview.quiet"))

# The stored plot with its theme back from the collection's store.
plots_plot <- function(plots, i) {
  meta <- plots_meta_raw(plots)
  load_packages(meta$packages)
  plot <- plots$plot[[i]]
  theme <- meta$themes[[plots$theme[[i]]]]
  if (is.null(theme)) {
    cli::cli_abort(c(
      "The theme of {.val {plots$name[[i]]}} is not in the collection.",
      "i" = "A collection saved by an earlier ggview has to be built again."
    ), call = NULL)
  }
  # Anything added to the stored plot since goes on top of the stored theme.
  plot$theme <- if (length(plot$theme)) theme + plot$theme else theme
  plot
}

# A theme element drawn by another package, such as a ggtext textbox, is plain
# data: nothing in it makes reading the collection load that package.
theme_packages <- function(theme) {
  classes <- unique(unlist(lapply(theme, class)))
  found <- vapply(classes, function(cls) {
    method <- utils::getS3method("element_grob", cls, optional = TRUE,
                                 envir = asNamespace("ggplot2"))
    if (is.null(method)) "" else environmentName(topenv(environment(method)))
  }, character(1))
  setdiff(unique(found), c("", "base", "ggplot2", "R_GlobalEnv"))
}

load_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    cli::cli_abort(c(
      "Drawing these plots needs {.pkg {missing}}, which {?is/are} not installed.",
      "i" = "Install {?it/them}, then read the collection again."
    ), call = NULL)
  }
}

# Themes no plot uses any more are dropped, so a filtered collection carries
# only its own.
keep_used_themes <- function(meta, x) {
  meta$themes <- meta$themes[intersect(names(meta$themes), unique(x$theme))]
  meta
}

plots_meta_raw <- function(x) {
  attr(x, "meta") %||% plots_meta_default()
}

# The customizer a plot names, able to call the collection's functions.
plots_customizer <- function(plots, i) {
  key <- plots$customizer[[i]]
  if (!is.character(key) || length(key) != 1L) {
    cli::cli_abort(c(
      "{.val {plots$name[[i]]}} does not name a customizer.",
      "i" = "A collection saved by an earlier ggview has to be built again."
    ), call = NULL)
  }
  meta <- plots_meta_raw(plots)
  customizer <- meta$customizers[[key]]
  if (is.null(customizer)) {
    cli::cli_abort(c(
      "{.val {plots$name[[i]]}} uses the customizer {.val {key}}, which the collection
       does not have.",
      "i" = "It has {.val {names(meta$customizers)}}."
    ), call = NULL)
  }
  bind_functions(customizer, meta$functions)
}

# Keep the class and the collection's own details through dplyr verbs, as long
# as the result still holds plots. A summary such as `count()` does not, and is
# a plain tibble.
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.plots_tbl <- function(data, template) {
  restore_plots_tbl(NextMethod(), template)
}

plots_columns <- c("id", "name", "type", "width", "height", "plot", "customizer",
                   "theme", "values")

restore_plots_tbl <- function(out, template) {
  if (!is.data.frame(out)) return(out)
  if (!all(plots_columns %in% names(out))) return(as_tibble.plots_tbl(out))
  new_plots_tbl(out, meta = keep_used_themes(plots_meta_raw(template), out))
}

# `as_tibble()` is the way out of the listing: it gives the plain tibble, which
# prints as any tibble does, list-columns and all.
#' @exportS3Method tibble::as_tibble
as_tibble.plots_tbl <- function(x, ...) {
  class(x) <- setdiff(class(x), "plots_tbl")
  attr(x, "meta") <- NULL
  x
}

#' @export
`[.plots_tbl` <- function(x, ...) {
  restore_plots_tbl(NextMethod(), x)
}

# An id only has to be unique within its collection, and tempfile() gives one
# without touching the session's random seed.
plots_new_id <- function(taken = character()) {
  repeat {
    id <- basename(tempfile(pattern = ""))
    if (!id %in% taken) return(id)
  }
}

# Row index of one plot. `.last = TRUE` allows both keys to be missing.
plots_locate <- function(plots, name = NULL, id = NULL, .last = FALSE,
                         call = parent.frame()) {
  if (is.null(name) && is.null(id)) {
    if (!.last) {
      cli::cli_abort("Give a {.arg name} or an {.arg id}.", call = call)
    }
    if (nrow(plots) == 0) {
      cli::cli_abort(
        c("The collection is empty.", "i" = "Add a plot with {.fn plots_append}."),
        call = call
      )
    }
    return(nrow(plots))
  }
  key <- name %||% id
  if (length(key) != 1L) {
    cli::cli_abort("Name exactly one plot, not {length(key)}.", call = call)
  }
  plots_locate_all(plots, name, id, call = call)
}

# Row indices, for the vectorised keys.
plots_locate_all <- function(plots, name = NULL, id = NULL, call = parent.frame()) {
  if (is.null(name) && is.null(id)) {
    cli::cli_abort("Give a {.arg name} or an {.arg id}.", call = call)
  }
  if (!is.null(name) && !is.null(id)) {
    cli::cli_abort("Give a {.arg name} or an {.arg id}, not both.", call = call)
  }
  arg <- if (is.null(name)) "id" else "name"
  key <- name %||% id
  if (!is.character(key) || !length(key) || anyNA(key)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector without {.val NA}.",
      call = call
    )
  }

  i <- match(key, if (is.null(name)) plots$id else plots$name)
  if (anyNA(i)) {
    cli::cli_abort(
      c(
        "No plot with {arg} {.val {key[is.na(i)]}}.",
        "i" = "The collection holds {nrow(plots)} plot{?s}: {.code print(plots)} lists them."
      ),
      call = call
    )
  }
  i
}

# --- default canvas sizes ----------------------------------------------------

#' @title Default canvas size per plot type
#' @description The canvas size [plots_append()] gives a plot when the plot
#'   brings none of its own, and the size [plots_reset()] goes back to. Pass the
#'   result to [plots_init()] to change the defaults for one collection.
#'
#' @param ... Named sizes, each a numeric `c(width, height)` in inches, for
#'   example `heatmap = c(18, 11)`. They replace the built-in default for those
#'   types and leave the rest alone.
#' @param .default Size for a plot whose type has no entry.
#'
#' @return A named list of sizes, of class `plots_dims`.
#'
#' @examples
#' plots_dims()
#' plots_dims(heatmap = c(18, 11), bar = c(10, 6))
#'
#' @export
plots_dims <- function(..., .default = c(15, 9)) {
  defaults <- list(
    bar         = c(12, 8),
    diverging   = c(15, 8),
    dodged_bar  = c(15, 9),
    dumbbell    = c(16, 9),
    heatmap     = c(16, 10),
    line        = c(15, 9),
    stacked_bar = c(15, 9)
  )
  sizes <- list(...)
  if (length(sizes) && !all(nzchar(names(sizes) %||% ""))) {
    cli::cli_abort("Every size in {.arg ...} must be named after a plot type.")
  }
  for (type in names(sizes)) check_dim(sizes[[type]], type)
  check_dim(.default, ".default")

  out <- utils::modifyList(defaults, lapply(sizes, as.double))
  out <- out[order(names(out))]
  out[[".default"]] <- as.double(.default)
  structure(out, class = "plots_dims")
}

# A `canvas()` already on a plot, in inches. The collection works in inches, so a
# canvas in centimetres or pixels is converted rather than read as a number.
canvas_inches <- function(canvas) {
  if (is.null(canvas)) return(NULL)
  unit <- (canvas$units %||% "in")[[1]]
  per_inch <- switch(unit, "in" = 1, "cm" = 2.54, "mm" = 25.4, "px" = canvas$dpi %||% 300)
  if (is.null(per_inch)) return(NULL)
  list(width = canvas$width / per_inch, height = canvas$height / per_inch)
}

# The canvas, offered the way a customizer offers its own parameters. The size the
# script declared is the default, and anything set later is a change on top of it.
geometry_params <- function(width, height) {
  list(
    # 30 inches is already a large slide. ggplot2 refuses to save past 50.
    width  = param_number("Width (in)", default = width, min = 1, max = 30, step = 0.5),
    height = param_number("Height (in)", default = height, min = 1, max = 30, step = 0.5)
  )
}

# The canvas a plot renders at: what was set, else what was declared.
plots_size <- function(plots, i) {
  values <- plots$values[[i]]
  list(
    width = values[["width"]] %||% plots$width[[i]],
    height = values[["height"]] %||% plots$height[[i]]
  )
}

plots_dims_lookup <- function(dims, type) {
  size <- if (!is.na(type)) dims[[type]] else NULL
  size <- size %||% dims[[".default"]]
  list(width = size[[1]], height = size[[2]])
}

# --- validation --------------------------------------------------------------

check_plots <- function(plots, call = parent.frame()) {
  if (!inherits(plots, "plots_tbl")) {
    cli::cli_abort(
      c(
        "{.arg plots} must be a {.cls plots_tbl}, not {.cls {class(plots)[[1]]}}.",
        "i" = "Start a collection with {.fn plots_init}."
      ),
      call = call
    )
  }
  invisible(plots)
}

check_name <- function(name, call = parent.frame()) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.", call = call)
  }
  if (grepl("^/|/$|//", name)) {
    cli::cli_abort(
      c("{.arg name} has an empty folder in it: {.val {name}}.",
        "i" = "A slash separates folders, so it needs a name on each side."),
      call = call
    )
  }
  invisible(name)
}

check_line <- function(text, arg, call = parent.frame()) {
  if (is.null(text)) return(invisible(NULL))
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_abort("{.arg {arg}} must be a single string, or {.code NULL}.", call = call)
  }
  invisible(text)
}

# A folder relative to wherever the files are written: no slash at either end,
# no empty folder, and no `.` or `..`, which would leave that root.
check_path <- function(path, call = parent.frame()) {
  if (is.null(path)) return(invisible(NULL))
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty string, or {.code NULL}.",
                   call = call)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (grepl("\\\\", path) || grepl("^/|/$|//", path) || any(parts %in% c(".", ".."))) {
    cli::cli_abort(
      c("{.arg path} must be a relative folder: {.val {path}}.",
        "i" = "Separate folders with {.code /}, with none at either end and no {.code .} or {.code ..},
               for example {.val results/2026-09/plots}."),
      call = call
    )
  }
  invisible(path)
}

check_type <- function(type, call = parent.frame()) {
  if (length(type) != 1L || !(is.character(type) || is.na(type))) {
    cli::cli_abort("{.arg type} must be a single string, or {.val NA}.", call = call)
  }
  invisible(type)
}

check_size <- function(size, arg, call = parent.frame()) {
  if (!is.numeric(size) || length(size) != 1L || is.na(size) || size <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive number of inches.", call = call)
  }
  as.double(size)
}

check_dim <- function(size, type, call = parent.frame()) {
  if (!is.numeric(size) || length(size) != 2L || anyNA(size) || any(size <= 0)) {
    cli::cli_abort(
      "Size for {.field {type}} must be two positive numbers, {.code c(width, height)}.",
      call = call
    )
  }
  invisible(size)
}

check_dims <- function(dims, call = parent.frame()) {
  if (!inherits(dims, "plots_dims")) {
    cli::cli_abort(
      c(
        "{.arg dims} must come from {.fn plots_dims}.",
        "i" = "For example {.code plots_init(dims = plots_dims(heatmap = c(18, 11)))}."
      ),
      call = call
    )
  }
  invisible(dims)
}

# Changes, checked against the parameters the plot's customizer offers.
check_values <- function(values, customizer, plot, geometry = list(),
                         existing = list(), call = parent.frame()) {
  if (!length(values)) return(list())
  if (!length(names(values)) || !all(nzchar(names(values)))) {
    cli::cli_abort("Every change must be named after a parameter.", call = call)
  }

  params <- c(customizer_params(customizer, plot, call = call), geometry)
  unknown <- setdiff(names(values), names(params))
  if (length(unknown)) {
    cli::cli_abort(
      c(
        "This plot has no parameter {.val {unknown}}.",
        "i" = "It takes {.val {names(params)}}.",
        "i" = "{.code plots_params(plots, {.val {'<name>'}})} lists them in full."
      ),
      call = call
    )
  }
  for (key in names(values)) {
    param <- params[[key]]
    value <- param_check(param, values[[key]], key, call = call)
    # A mapping is always whole: changing one key keeps the rest, so a customizer
    # never receives a palette with holes in it.
    if (identical(param$type, "mapping") && is.list(value)) {
      whole <- existing[[key]] %||% param$default
      if (length(whole)) value <- utils::modifyList(as.list(whole), value)
    }
    values[[key]] <- value
  }
  values
}

# The recorded changes a replacement plot still accepts. A change it no longer
# takes (a parameter gone, a key no longer on the plot) is dropped, and said so.
values_still_valid <- function(values, customizer, plot, width, height) {
  if (!length(values)) return(list())
  params <- c(customizer_params(customizer, plot), geometry_params(width, height))
  ok <- vapply(names(values), function(key) {
    !is.null(params[[key]]) &&
      !inherits(try(param_check(params[[key]], values[[key]], key), silent = TRUE),
                "try-error")
  }, logical(1))
  if (!all(ok)) {
    dropped <- names(values)[!ok]
    cli::cli_alert_warning(
      "Dropped {length(dropped)} change{?s} the new plot no longer takes: {.val {dropped}}."
    )
  }
  values[ok]
}

# A collection's functions: named, each a function, and free of anything that
# only exists in this session. Their source records go, as a customizer's do.
check_functions <- function(functions, call = parent.frame()) {
  check_named_list(functions, "functions", call = call)
  for (key in names(functions)) {
    fun <- functions[[key]]
    if (!is.function(fun) || is.primitive(fun)) {
      cli::cli_abort("{.arg functions} must hold functions: {.val {key}} is not one.",
                     call = call)
    }
    check_travels(fun, cli::format_inline("Function {.val {key}}"),
                  known = names(functions), call = call)
    functions[[key]] <- utils::removeSource(fun)
  }
  functions
}

check_customizers <- function(customizers, known, call = parent.frame()) {
  check_named_list(customizers, "customizers", call = call)
  for (key in names(customizers)) {
    customizer <- customizers[[key]]
    if (!inherits(customizer, "plots_customizer")) {
      cli::cli_abort(
        c("{.arg customizers} must hold customizers: {.val {key}} is not one.",
          "i" = "Build one with {.fn customizer} or {.fn customizer_extend}."),
        call = call
      )
    }
    what <- cli::format_inline("Customizer {.val {key}}")
    for (layer in customizer$layers) {
      check_travels(layer$apply, what, known = known, call = call)
      check_travels(layer$params, what, known = known, call = call)
    }
  }
  customizers
}

check_named_list <- function(x, arg, call = parent.frame()) {
  if (!is.list(x) || inherits(x, "plots_customizer")) {
    cli::cli_abort("{.arg {arg}} must be a named list.", call = call)
  }
  keys <- names(x) %||% rep("", length(x))
  if (!all(nzchar(keys)) || anyDuplicated(keys)) {
    cli::cli_abort("Every entry in {.arg {arg}} needs a name of its own.", call = call)
  }
  invisible(x)
}

# A plot carries functions of its own: a scale's labels or breaks, a layer's
# data, a stat's `fun`, a facet's labeller, and the calls in its mappings. One
# that reaches for this session, or for a package the reader may not attach,
# draws here and fails wherever the collection is read.
warn_plot_travel <- function(plot, name) {
  risks <- character()
  check <- function(where, fun) {
    found <- unique(unlist(lapply(wrapped_functions(fun), travel_risks)))
    if (length(found)) risks[[where]] <<- paste(found, collapse = ", ")
  }
  for (scale in plot$scales$scales) {
    for (field in c("labels", "breaks", "minor_breaks", "limits", "oob", "rescaler", "palette")) {
      value <- tryCatch(scale[[field]], error = function(e) NULL)
      if (is.function(value)) check(paste(scale$aesthetics[[1]], "scale", field), value)
    }
  }
  for (j in seq_along(plot$layers)) {
    layer <- plot$layers[[j]]
    if (is.function(layer$data)) check(paste("layer", j, "data"), layer$data)
    for (part in c("stat_params", "geom_params", "aes_params")) {
      for (key in names(layer[[part]])) {
        if (is.function(layer[[part]][[key]])) check(paste("layer", j, key), layer[[part]][[key]])
      }
    }
  }
  labeller <- tryCatch(plot$facet$params$labeller, error = function(e) NULL)
  if (is.function(labeller)) check("facet labeller", labeller)
  mappings <- c(list(plot = plot$mapping),
                stats::setNames(lapply(plot$layers, function(l) l$mapping),
                                paste("layer", seq_along(plot$layers))))
  for (where in names(mappings)) {
    for (aesthetic in names(mappings[[where]])) {
      q <- mappings[[where]][[aesthetic]]
      if (!rlang::is_quosure(q)) next
      found <- mapping_risks(q)
      if (length(found)) risks[[paste(where, aesthetic)]] <- paste(found, collapse = ", ")
    }
  }
  if (length(risks)) {
    cli::cli_warn(c(
      "{.val {name}} carries functions that will not work where the collection is read:",
      stats::setNames(paste0(names(risks), ": ", risks), rep("x", length(risks))),
      "i" = "Qualify each one, such as {.code stringr::str_wrap()}, or use {.pkg scales}
             helpers such as {.code scales::label_wrap()}."
    ), call = NULL)
  }
  invisible(plot)
}

# A function and the ones it wraps. ggplot2 keeps a scale's `labels` inside a
# closure of its own, so the function given sits one environment down.
wrapped_functions <- function(fun, depth = 2L) {
  out <- list(fun)
  env <- environment(fun)
  if (depth < 1L || is.null(env) || isNamespace(env) || identical(env, globalenv()) ||
      identical(env, emptyenv()) || startsWith(environmentName(env), "package:")) {
    return(out)
  }
  for (name in ls(env, all.names = TRUE)) {
    value <- tryCatch(get(name, envir = env, inherits = FALSE), error = function(e) NULL)
    if (is.function(value) && !is.primitive(value)) {
      out <- c(out, wrapped_functions(value, depth - 1L))
    }
  }
  out
}

# --- small helpers -----------------------------------------------------------

# A label the collection can show: a single string, or NA when the plot has
# none, or has one ggplot2 does not render as plain text.
plot_label <- function(plot, slot) {
  label <- plot$labels[[slot]]
  if (is.null(label) && slot %in% c("x", "y")) label <- plot_label_built(plot, slot)
  if (is.character(label) && length(label) == 1L) label else NA_character_
}

# An axis title usually comes from the mapping, and a plot only works that out
# when it is built. Older ggplot2 versions cannot say, and report none.
plot_label_built <- function(plot, slot) {
  get_labs <- tryCatch(getExportedValue("ggplot2", "get_labs"), error = function(e) NULL)
  if (is.null(get_labs)) return(NULL)
  tryCatch(get_labs(plot)[[slot]], error = function(e) NULL)
}
