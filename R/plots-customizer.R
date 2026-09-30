# What somebody may change about a plot, and what changing it does.

# Geometry is not customizable: the collection owns it.
plots_reserved <- c("width", "height")

#' @title Describe how a plot can be customized
#' @description A customizer is a pair of functions. `apply` takes a stored
#'   plot and the values somebody changed, and returns the changed plot.
#'   `params` takes the stored plot and returns the parameters it accepts,
#'   each one built with a `param_*()` function.
#'
#'   Keeping the two together is what lets a program offer the right control
#'   for each parameter without knowing anything about plots: [plots_params()]
#'   answers what may be changed, and [plots_get()] applies it.
#'
#'   [customizer_default()] handles the labels every plot has. Write your own
#'   for anything else — a palette, a set of categories to show, a text size.
#'   A customizer may rebuild a plot from its data rather than add to it, so
#'   what it can offer is not limited to what `ggplot2` lets you add on top.
#'
#'   Give it to [plots_init()] under a name, once, and let each plot name the
#'   customizer it uses. It can call any function the collection carries in
#'   `functions`. Anything else it uses must come with it: a package function,
#'   or a value it captured.
#'
#' @param apply A function whose first argument is the plot. Its other
#'   arguments are the parameters, and each one must default to `NULL`, meaning
#'   "leave this alone". It returns a ggplot.
#' @param params A function of the plot, returning a named list of `param_*()`
#'   objects. The names are the argument names of `apply`.
#'
#' @return A customizer.
#'
#' @seealso [customizer_default()], [customizer_extend()], [param_text()],
#'   [plots_params()].
#'
#' @examples
#' library(ggplot2)
#'
#' # A customizer that recolors a plot, offering one color per level.
#' recolor <- customizer(
#'   apply = function(plot, colors = NULL) {
#'     if (is.null(colors)) return(plot)
#'     suppressMessages(plot + scale_fill_manual(values = unlist(colors)))
#'   },
#'   params = function(plot) {
#'     list(colors = param_mapping("Colors", keys = c("a", "b"), to = "color"))
#'   }
#' )
#' recolor
#'
#' @export
customizer <- function(apply, params) {
  check_customizer_fun(apply, "apply")
  check_customizer_fun(params, "params")
  keys <- names(formals(apply))[-1]
  check_reserved(keys)
  # A source record can carry the whole source of the package that made the
  # function, so a saved collection would carry it too.
  new_customizer(list(list(
    apply = utils::removeSource(apply), params = utils::removeSource(params), keys = keys
  )))
}

# A customizer is a stack of layers, each an `apply`, a `params` and the keys
# its `apply` takes. Extending one adds a layer, so no function has to hold
# another customizer to call it.
new_customizer <- function(layers) {
  structure(list(layers = layers), class = "plots_customizer")
}

customizer_keys <- function(customizer) {
  unique(unlist(lapply(customizer$layers, function(layer) layer$keys)))
}

#' @export
print.plots_customizer <- function(x, ...) {
  cli::cli_rule(left = "{.cls plots_customizer}")
  keys <- customizer_keys(x)
  keys[keys == "..."] <- "set by the plot"
  cli::cli_verbatim(paste0("  parameters: ", paste(keys, collapse = ", ")))
  invisible(x)
}

#' @title The customizer every plot starts with
#' @description Offers what any plot has, whatever it draws: its title,
#'   subtitle, footnote and axis titles, the size of each of those, the size and
#'   line wrapping of the axis text, the overall text size, where the legend sits
#'   and which way it runs, whether the gridlines show, a color for each
#'   category the plot colors by, and a new name for each legend entry and each
#'   category on a discrete axis. Each one defaults to what the plot already
#'   carries, `NA` leaves it alone, and not mentioning it changes nothing.
#'
#'   A new name changes only the label: the category, its color and its place
#'   stay. The names go on the scale, in place, so a wrapped axis wraps them
#'   too. An axis with more than 60 categories offers none.
#'
#'   The overall text size scales every text size the theme sets in points by
#'   the same ratio, so it works on a complete theme too, and the text the plot
#'   draws in its panel, such as the numbers on bars, with it. A specific size
#'   given with it wins: `data_label_size` sets that panel text in points. A
#'   layer that maps its size to the data keeps its own sizes. Showing a grid brings back its major lines only, drawn like
#'   the plot's other gridlines when it shows some.
#'
#'   The colors come from the plot's discrete fill scale, or its colour scale
#'   when it has no fill. A change reaches every such scale that holds the same
#'   category, so text drawn in its bar's color follows the bar. The scale is
#'   changed in place, so its legend title and guide stay. A continuous scale,
#'   such as a heatmap's, offers no colors.
#'
#'   Sizes are changed in place, so a title drawn as a `ggtext` textbox stays a
#'   textbox. Replacing it outright is an error in ggplot2, not a silent loss.
#'
#' @return A customizer.
#'
#' @seealso [customizer()] to write one of your own.
#'
#' @examples
#' customizer_default()
#'
#' @export
customizer_default <- function() {
  new_customizer(c(
    # First, so the labels layer wraps the new names.
    customizer(apply = customize_names, params = params_names)$layers,
    customizer(apply = customize_labels, params = params_labels)$layers,
    customizer(apply = customize_colors, params = params_colors)$layers
  ))
}

legend_positions <- c("right", "left", "top", "bottom", "none")
legend_directions <- c("horizontal", "vertical")

# The theme element each size parameter changes.
size_elements <- c(
  title_size = "plot.title", subtitle_size = "plot.subtitle",
  caption_size = "plot.caption", x_size = "axis.title.x",
  y_size = "axis.title.y", text_size = "text"
)

customize_labels <- function(plot,
                             title = NULL, title_size = NULL,
                             subtitle = NULL, subtitle_size = NULL,
                             caption = NULL, caption_size = NULL,
                             x = NULL, x_size = NULL,
                             y = NULL, y_size = NULL,
                             axis_text_size = NULL, axis_text_wrap = NULL,
                             text_size = NULL, data_label_size = NULL,
                             legend = NULL, legend_direction = NULL,
                             x_grid = NULL, y_grid = NULL) {
  labels <- list(title = title, subtitle = subtitle, caption = caption, x = x, y = y)
  labels <- labels[!vapply(labels, is.null, logical(1))]
  if (length(labels)) {
    # NA is "take this label away", which ggplot2 spells as NULL.
    labels <- lapply(labels, function(value) if (is.na(value)) NULL else value)
    plot <- plot + do.call(ggplot2::labs, labels)
  }

  # The overall size first, so a specific size given alongside it wins.
  if (given(text_size)) plot <- text_resize(plot, text_size)
  sizes <- list(title_size = title_size, subtitle_size = subtitle_size,
                caption_size = caption_size, x_size = x_size, y_size = y_size)
  for (key in names(sizes)) {
    if (given(sizes[[key]])) plot <- element_resize(plot, size_elements[[key]], sizes[[key]])
  }
  if (given(data_label_size)) plot <- resize_labels(plot, label_layers(plot), data_label_size)

  # A theme that sets both axis texts makes the parent element a no-op, so set both.
  if (given(axis_text_size)) {
    plot <- element_resize(plot, "axis.text.x", axis_text_size)
    plot <- element_resize(plot, "axis.text.y", axis_text_size)
  }
  if (given(axis_text_wrap)) {
    for (aesthetic in c("x", "y")) plot <- wrap_axis(plot, aesthetic, axis_text_wrap)
  }

  settings <- list()
  if (given(legend)) settings$legend.position <- legend
  if (given(legend_direction)) settings$legend.direction <- legend_direction
  # Showing a grid brings back its major lines only. Hiding it hides both.
  if (given(x_grid)) {
    settings$panel.grid.major.x <- grid_element(plot, "x", x_grid)
    if (!x_grid) settings$panel.grid.minor.x <- ggplot2::element_blank()
  }
  if (given(y_grid)) {
    settings$panel.grid.major.y <- grid_element(plot, "y", y_grid)
    if (!y_grid) settings$panel.grid.minor.y <- ggplot2::element_blank()
  }
  if (length(settings)) plot <- plot + do.call(ggplot2::theme, settings)

  plot
}

params_labels <- function(plot) {
  theme <- plot_theme(plot)
  # A text the theme hides has no size to offer.
  size <- function(key) {
    if (element_hidden(theme, size_elements[[key]])) return(NULL)
    param_number(size_labels[[key]], default = element_size(theme, size_elements[[key]]),
                 min = 4, max = 80, step = 1)
  }
  axis_text <- setdiff(c("axis.text.x", "axis.text.y"),
                       Filter(function(name) element_hidden(theme, name), c("axis.text.x", "axis.text.y")))

  params <- list(
    title            = label_param("Title", plot, "title"),
    title_size       = size("title_size"),
    subtitle         = label_param("Subtitle", plot, "subtitle", multiline = TRUE),
    subtitle_size    = size("subtitle_size"),
    caption          = label_param("Footnote", plot, "caption", multiline = TRUE),
    caption_size     = size("caption_size"),
    x                = label_param("X axis title", plot, "x"),
    x_size           = size("x_size"),
    y                = label_param("Y axis title", plot, "y"),
    y_size           = size("y_size"),
    axis_text_size   = if (length(axis_text)) {
                         param_number("Axis text size", default = element_size(theme, axis_text[[1]]),
                                      min = 4, max = 80, step = 1)
                       },
    axis_text_wrap   = param_number("Axis text wrap (characters)",
                                    min = 5, max = 120, step = 1),
    text_size        = size("text_size"),
    data_label_size  = local({
      layers <- label_layers(plot)
      if (length(layers)) {
        param_number("Data label size", default = round(layers[[1]]$points * 2) / 2,
                     min = 2, max = 80, step = 0.5)
      }
    }),
    legend           = param_choice("Legend", choices = legend_positions,
                                    default = legend_position(plot)),
    legend_direction = param_choice("Legend direction", choices = legend_directions,
                                    default = theme_string(plot, "legend.direction")),
    x_grid           = param_flag("X gridlines", default = grid_shown(theme, "x")),
    y_grid           = param_flag("Y gridlines", default = grid_shown(theme, "y"))
  )
  params[!vapply(params, is.null, logical(1))]
}

# A label's text parameter. It runs over several lines when it has to: a
# subtitle or footnote, or any label that already breaks.
label_param <- function(label, plot, slot, multiline = FALSE) {
  default <- plot_label(plot, slot)
  param_text(label, default = default,
             multiline = multiline || isTRUE(grepl("\n", default, fixed = TRUE)))
}

size_labels <- c(
  title_size = "Title size", subtitle_size = "Subtitle size",
  caption_size = "Footnote size", x_size = "X axis title size",
  y_size = "Y axis title size", text_size = "Text size"
)

# A value somebody actually asked for. `NULL` is "leave it alone" and so is `NA`
# for a setting that has no empty state, such as a legend position.
given <- function(value) !is.null(value) && !is.na(value)

# Change one theme element's size without changing what kind of element it is.
# A title drawn as a markdown textbox stays a textbox: ggplot2 refuses to merge
# two kinds of element, so replacing it outright is an error. An element the
# theme hides stays hidden: it has no size, and a new one would draw it again.
element_resize <- function(plot, name, size) {
  if (element_hidden(plot_theme(plot), name)) return(plot)
  current <- plot$theme[[name]]
  element <- if (inherits(current, "element")) {
    current$size <- size
    current
  } else {
    ggplot2::element_text(size = size)
  }
  plot + do.call(ggplot2::theme, stats::setNames(list(element), name))
}

# Change the overall text size. A theme that gives an element its own size in
# points would ignore the root `text` element, so every such size is scaled by the
# same ratio. Sizes given as `rel()` follow the root on their own.
text_resize <- function(plot, size) {
  current <- element_size(plot_theme(plot), "text")
  plot <- element_resize(plot, "text", size)
  if (is.null(current) || current <= 0) return(plot)
  ratio <- size / current
  own <- vapply(plot$theme, function(element) {
    inherits(element, "element_text") && is.numeric(element$size) &&
      !inherits(element$size, "rel")
  }, logical(1))
  for (name in setdiff(names(plot$theme)[own], "text")) {
    plot <- element_resize(plot, name, plot$theme[[name]]$size * ratio)
  }
  # The text drawn in the panel grows with the rest.
  resize_labels(plot, label_layers(plot), function(points) points * ratio)
}

# The layers that write text in the panel, such as the numbers on bars, each with
# the size it draws at now, in points. A layer that maps size to the data has no
# one size to change, so it is left out.
text_geoms <- c("GeomText", "GeomLabel", "GeomRichText", "GeomTextBox", "GeomTextRepel",
                "GeomLabelRepel")

points_per <- c(mm = 72.27 / 25.4, pt = 1, cm = 72.27 / 2.54, `in` = 72.27, pc = 12)

label_layers <- function(plot, built = NULL) {
  wanted <- which(vapply(plot$layers, function(l) any(class(l$geom) %in% text_geoms), logical(1)))
  if (!length(wanted)) return(list())
  built <- built %||% build_quietly(plot)
  out <- list()
  for (i in wanted) {
    sizes <- unique(built$data[[i]]$size)
    unit <- plot$layers[[i]]$geom_params$size.unit %||% "mm"
    if (length(sizes) != 1L || !is.numeric(sizes) || is.na(sizes) || is.na(points_per[unit])) next
    out[[length(out) + 1L]] <- list(index = i, unit = unit, points = sizes * points_per[[unit]])
  }
  out
}

# New sizes for those layers: a number of points, or a function of the points each
# has now. A layer is a shared object, so each one is replaced by a child that
# holds the new size, and the stored plot keeps its own.
resize_labels <- function(plot, layers, points) {
  for (l in layers) {
    size <- if (is.function(points)) points(l$points) else points
    plot$layers[[l$index]] <- resized_layer(plot$layers[[l$index]], size / points_per[[l$unit]])
  }
  plot
}

# ggproto() keeps its parent as a promise, so the parent is forced here: in a loop it
# would otherwise resolve to the last layer the loop saw.
resized_layer <- function(layer, size) {
  force(layer)
  params <- layer$aes_params
  params$size <- size
  ggplot2::ggproto(NULL, layer, aes_params = params)
}

# Break a discrete axis's labels over several lines. A scale the plot sets itself
# is copied and keeps everything it had (position, expansion, its own labels); only
# its labels are wrapped. Without one, the plot is built once to learn whether the
# axis is discrete at all.
wrap_axis <- function(plot, aesthetic, width) {
  scale <- plot$scales$get_scales(aesthetic)
  if (is.null(scale)) {
    built <- suppressMessages(ggplot2::ggplot_build(plot))
    trained <- built$plot$scales$get_scales(aesthetic)
    if (is.null(trained) || !isTRUE(trained$is_discrete())) return(plot)
    build <- if (aesthetic == "x") ggplot2::scale_x_discrete else ggplot2::scale_y_discrete
    return(suppressMessages(plot + build(labels = wrap_labels(width))))
  }
  if (!isTRUE(scale$is_discrete())) return(plot)

  scale <- scale$clone()
  wrap <- wrap_labels(width)
  own <- scale$labels
  scale$labels <- if (is.function(own)) {
    function(x) wrap(own(x))
  } else if (is.character(own)) {
    stats::setNames(wrap(own), names(own))
  } else {
    wrap
  }
  suppressMessages(plot + scale)
}

wrap_labels <- function(width) {
  function(x) {
    vapply(
      as.character(x),
      function(label) paste(strwrap(label, width = width), collapse = "\n"),
      character(1), USE.NAMES = FALSE
    )
  }
}

# A gridline that can be seen. An empty line would inherit the theme's parent
# gridline, which a theme may draw in the panel's own color: white on white. So a
# grid comes back drawn like the axis's own line or the other axis's, then the
# parent's, and a light grey when none of them shows on the panel.
grid_element <- function(plot, axis, show) {
  if (!isTRUE(show)) return(ggplot2::element_blank())
  theme <- plot_theme(plot)
  other <- if (axis == "x") "y" else "x"
  for (name in c(paste0("panel.grid.major.", c(axis, other)), "panel.grid.major", "panel.grid")) {
    line <- visible_line(theme, name)
    if (!is.null(line)) return(line)
  }
  width <- tryCatch(ggplot2::calc_element("panel.grid", theme)$linewidth, error = function(e) NULL)
  ggplot2::element_line(colour = "grey92", linewidth = width %||% 0.5, inherit.blank = FALSE)
}

# The line a theme element draws, when it draws one the panel does not hide.
visible_line <- function(theme, name) {
  if (is.null(theme)) return(NULL)
  line <- tryCatch(ggplot2::calc_element(name, theme), error = function(e) NULL)
  if (!inherits(line, "element_line") || is.null(line$colour) || is.na(line$colour)) {
    return(NULL)
  }
  if (same_color(line$colour, panel_fill(theme))) return(NULL)
  ggplot2::element_line(colour = line$colour, linewidth = line$linewidth,
                        linetype = line$linetype, lineend = line$lineend,
                        inherit.blank = FALSE)
}

# What a gridline is drawn on: the panel, or the plot behind it when the panel is
# blank or see-through.
panel_fill <- function(theme) {
  for (name in c("panel.background", "plot.background")) {
    fill <- tryCatch(ggplot2::calc_element(name, theme)$fill, error = function(e) NULL)
    if (is.character(fill) && length(fill) == 1L && !is.na(fill) &&
        grDevices::col2rgb(fill, alpha = TRUE)[[4]] > 0) {
      return(fill)
    }
  }
  "white"
}

same_color <- function(a, b) {
  identical(grDevices::col2rgb(a), grDevices::col2rgb(b))
}

# The plot's theme filled in from the defaults it inherits, for reading what a
# setting currently is. Older ggplot2 versions cannot say, and report nothing.
plot_theme <- function(plot) {
  tryCatch(ggplot2::complete_theme(plot$theme), error = function(e) NULL)
}

# Whether the theme hides an element, itself or through the one it inherits from.
element_hidden <- function(theme, name) {
  if (is.null(theme)) return(FALSE)
  inherits(tryCatch(ggplot2::calc_element(name, theme), error = function(e) NULL), "element_blank")
}

element_size <- function(theme, name) {
  if (is.null(theme)) return(NULL)
  size <- tryCatch(ggplot2::calc_element(name, theme)$size, error = function(e) NULL)
  if (is.numeric(size) && length(size) == 1L) size else NULL
}

grid_shown <- function(theme, axis) {
  if (is.null(theme)) return(NULL)
  !is.null(visible_line(theme, paste0("panel.grid.major.", axis)))
}

# --- category names ------------------------------------------------------------

# More categories than this on one axis is a list nobody edits one by one.
max_named <- 60L

build_quietly <- function(plot) suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot)))

# A new label for each legend entry and each category on a discrete axis, each
# starting from what the plot shows now. An axis flipped by `coord_flip()` is
# named for where it is drawn.
params_names <- function(plot) {
  built <- build_quietly(plot)
  out <- list()
  legend <- color_scales(plot, built)
  if (length(legend)) {
    main <- legend[[1]]
    out$legend_labels <- param_mapping(
      "Legend labels", keys = main$keys, to = "text",
      default = as.list(stats::setNames(main$labels, main$keys))
    )
  }
  flipped <- inherits(plot$coordinates, "CoordFlip")
  for (aesthetic in c("x", "y")) {
    axis <- axis_categories(built, aesthetic)
    if (is.null(axis)) next
    drawn <- if (flipped) setdiff(c("x", "y"), aesthetic) else aesthetic
    out[[paste0(aesthetic, "_labels")]] <- param_mapping(
      paste(toupper(drawn), "axis labels"), keys = axis$keys, to = "text",
      default = as.list(stats::setNames(axis$labels, axis$keys))
    )
  }
  out
}

customize_names <- function(plot, legend_labels = NULL, x_labels = NULL, y_labels = NULL) {
  if (given_mapping(legend_labels)) {
    wanted <- unlist(legend_labels)
    scales <- color_scales(plot, build_quietly(plot))
    for (aesthetic in names(scales)) {
      scale <- scales[[aesthetic]]
      keep <- intersect(names(wanted), scale$keys)
      if (!length(keep)) next
      labels <- stats::setNames(scale$labels, scale$keys)
      labels[keep] <- wanted[keep]
      plot <- suppressMessages(plot + relabeled_scale(plot, aesthetic, labels))
    }
  }
  axes <- list(x = x_labels, y = y_labels)
  for (aesthetic in names(axes)) {
    if (given_mapping(axes[[aesthetic]])) {
      plot <- suppressMessages(plot + relabeled_scale(plot, aesthetic, unlist(axes[[aesthetic]])))
    }
  }
  plot
}

# A mapping someone set. `NA` leaves it alone, as it does every other setting.
given_mapping <- function(value) {
  !is.null(value) && !(length(value) == 1L && !is.list(value) && is.na(value))
}

# A discrete axis's categories and what the axis shows for each, from the built
# plot, where the axis is trained.
axis_categories <- function(built, aesthetic) {
  panel <- if (aesthetic == "x") built$layout$panel_scales_x else built$layout$panel_scales_y
  scale <- if (length(panel)) panel[[1]] else NULL
  if (is.null(scale) || !isTRUE(scale$is_discrete())) return(NULL)
  # An empty category, such as the one bar of `aes(x = "")`, has nothing to rename.
  keys <- as.character(scale$get_limits())
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys) || length(keys) > max_named) return(NULL)
  labels <- tryCatch(as.character(scale$get_labels(keys)), error = function(e) keys)
  if (length(labels) != length(keys)) labels <- keys
  list(keys = keys, labels = labels)
}

# The same scale with new labels, matched to its categories by name, so its
# colors, breaks and guide stay. A plot with no scale of its own gets the default
# discrete one, which is what it was drawing with.
relabeled_scale <- function(plot, aesthetic, labels) {
  own <- plot$scales$get_scales(aesthetic)
  if (is.null(own)) {
    build <- switch(aesthetic, x = ggplot2::scale_x_discrete, y = ggplot2::scale_y_discrete,
                    fill = ggplot2::scale_fill_discrete, colour = ggplot2::scale_colour_discrete)
    return(build(labels = labels))
  }
  scale <- own$clone()
  scale$labels <- labels
  scale
}

# --- colors ------------------------------------------------------------------

# One color per category, read off the plot's discrete fill scale, or its colour
# scale when it has no fill.
params_colors <- function(plot) {
  scales <- color_scales(plot)
  if (!length(scales)) return(list())
  main <- scales[[1]]
  list(colors = param_mapping(
    "Colors", keys = main$keys, to = "color",
    labels = if (identical(main$labels, main$keys)) NULL else main$labels,
    default = as.list(main$values)
  ))
}

# A change reaches every discrete fill and colour scale that holds the category.
customize_colors <- function(plot, colors = NULL) {
  if (is.null(colors) || (!is.list(colors) && length(colors) == 1L && is.na(colors))) {
    return(plot)
  }
  wanted <- unlist(colors)
  scales <- color_scales(plot)
  for (aesthetic in names(scales)) {
    scale <- scales[[aesthetic]]
    keep <- intersect(names(wanted), scale$keys)
    if (!length(keep)) next
    values <- scale$values
    values[keep] <- wanted[keep]
    plot <- suppressMessages(plot + recolored_scale(plot, aesthetic, values))
  }
  plot
}

# The plot's discrete color scales with at least one category, fill first: for
# each, its categories, the hex color each has now, and what the legend calls it.
color_scales <- function(plot, built = build_quietly(plot)) {
  out <- list()
  for (aesthetic in c("fill", "colour")) {
    scale <- built$plot$scales$get_scales(aesthetic)
    if (is.null(scale) || !isTRUE(scale$is_discrete())) next
    identity <- inherits(scale, "ScaleDiscreteIdentity")
    # An identity scale with no legend is never trained, so its categories are the
    # colors the layers draw.
    keys <- if (identity) {
      unique(unlist(lapply(built$data, function(d) d[[aesthetic]])))
    } else {
      scale$get_limits()
    }
    # An empty category cannot be a key: nothing can name it.
    keys <- as.character(keys[!is.na(keys)])
    keys <- keys[nzchar(keys)]
    if (!length(keys)) next
    values <- if (identity) keys else scale$map(keys)
    labels <- if (identity) keys else {
      tryCatch(as.character(scale$get_labels(keys)), error = function(e) keys)
    }
    if (length(labels) != length(keys)) labels <- keys
    out[[aesthetic]] <- list(keys = keys, values = stats::setNames(as_hex(values), keys),
                             labels = labels)
  }
  out
}

# The same scale with new colors, so its name, guide, breaks and labels stay. A
# plot with no scale of its own gets a manual one, which is all it had to lose.
recolored_scale <- function(plot, aesthetic, values) {
  manual <- if (aesthetic == "fill") ggplot2::scale_fill_manual else ggplot2::scale_colour_manual
  own <- plot$scales$get_scales(aesthetic)
  if (is.null(own)) return(manual(values = values))
  if (inherits(own, "ScaleDiscreteIdentity")) {
    return(manual(values = values, name = own$name, guide = own$guide))
  }
  scale <- own$clone()
  scale$palette <- function(n) values
  scale$palette.cache <- NULL
  scale$n.breaks.cache <- NULL
  scale
}

# A color as `#RRGGBB`, the form a color picker shows. `NA` stays `NA`.
as_hex <- function(colors) {
  vapply(colors, function(color) {
    if (is.na(color)) return(NA_character_)
    rgb <- grDevices::col2rgb(color)
    sprintf("#%02X%02X%02X", rgb[[1]], rgb[[2]], rgb[[3]])
  }, character(1), USE.NAMES = FALSE)
}

# --- the parameters a customizer can offer -----------------------------------

#' @title Add parameters to a customizer
#' @description Builds a customizer that offers everything `base` offers plus
#'   whatever `apply` and `params` add. Use it to keep the labels from
#'   [customizer_default()] while adding something of your own, rather than
#'   listing them again.
#'
#'   A parameter of the same name replaces the one it shadows: a change goes to
#'   the last layer that takes it.
#'
#' @param base The customizer to build on, usually [customizer_default()].
#' @inheritParams customizer
#'
#' @return A customizer.
#'
#' @examples
#' library(ggplot2)
#'
#' # The default labels, plus a switch for the legend's own title.
#' customizer_extend(
#'   customizer_default(),
#'   apply = function(plot, legend_title = NULL) {
#'     if (is.null(legend_title)) plot else plot + labs(color = legend_title)
#'   },
#'   params = function(plot) list(legend_title = param_text("Legend title"))
#' )
#'
#' @export
customizer_extend <- function(base, apply, params) {
  check_customizer(base)
  new_customizer(c(base$layers, customizer(apply, params)$layers))
}

# Apply changes layer by layer. Each change goes to the last layer that takes
# it, so an added parameter shadows the one it replaces.
customizer_apply <- function(customizer, plot, values) {
  layers <- customizer$layers
  owner <- vapply(names(values), function(key) {
    takes <- vapply(layers, function(layer) any(c(key, "...") %in% layer$keys), logical(1))
    if (any(takes)) max(which(takes)) else NA_integer_
  }, integer(1))
  if (anyNA(owner)) {
    cli::cli_abort(c(
      "No {.arg apply} of this customizer takes {.val {names(values)[is.na(owner)]}}.",
      "i" = "Its {.arg params} offers {?it/them}, so its {.arg apply} must take {?it/them} too."
    ), call = NULL)
  }
  for (j in seq_along(layers)) {
    plot <- do.call(layers[[j]]$apply, c(list(plot), values[owner == j]))
  }
  plot
}

#' @title Parameters a customizer offers
#' @description The kinds of parameter a [customizer()] can offer, one function
#'   per kind. A program reads the kind from [plots_params()] and shows the
#'   control that fits it: a box for text, a picker for a color, a list for a
#'   choice, one row per key for a mapping.
#'
#'   Every parameter carries a `default`, which is what the plot does without
#'   a change. It is what a reset goes back to.
#'
#' @param label What to call the parameter, for a person reading it.
#' @param default The value the plot already has. `NULL` when it has none.
#' @param choices The values allowed, for `param_choice()` and
#'   `param_choices()`.
#' @param keys The things being mapped, for `param_mapping()` — the levels of a
#'   plot's fill, say. Read them off the plot.
#' @param labels What to call each key, when the keys themselves read badly —
#'   `TRUE` and `FALSE`, say. One per key.
#' @param to What each key maps to: `"color"`, `"text"` or `"number"`.
#' @param min,max,step Bounds and increment for `param_number()`. Optional.
#' @param multiline Whether the text may run over several lines, as a caption
#'   often does, so a program offers a box rather than a single line.
#'
#' @return A parameter.
#'
#' @seealso [customizer()], [plots_params()].
#'
#' @examples
#' param_text("Title", default = "Brand funnel")
#' param_number("Text size", default = 11, min = 6, max = 40)
#' param_choice("Legend", choices = c("right", "bottom", "none"))
#' param_choices("Brands to show", choices = c("Ours", "Rival A", "Rival B"))
#' param_mapping("Colors", keys = c("Ours", "Rival A"), to = "color")
#' param_mapping("Colors", keys = c("TRUE", "FALSE"), labels = c("Ours", "Others"))
#'
#' @name param
NULL

#' @rdname param
#' @export
param_text <- function(label, default = NULL, multiline = FALSE) {
  if (!is.logical(multiline) || length(multiline) != 1L || is.na(multiline)) {
    cli::cli_abort("{.arg multiline} must be {.code TRUE} or {.code FALSE}.")
  }
  new_param("text", label, default, multiline = multiline)
}

#' @rdname param
#' @export
param_number <- function(label, default = NULL, min = NULL, max = NULL, step = NULL) {
  for (bound in c("min", "max", "step")) {
    value <- switch(bound, min = min, max = max, step = step)
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L || is.na(value))) {
      cli::cli_abort("{.arg {bound}} must be a single number, or {.code NULL}.")
    }
  }
  new_param("number", label, default, min = min, max = max, step = step)
}

#' @rdname param
#' @export
param_flag <- function(label, default = NULL) {
  new_param("flag", label, default)
}

#' @rdname param
#' @export
param_choice <- function(label, choices, default = NULL) {
  new_param("choice", label, default, choices = check_choices(choices))
}

#' @rdname param
#' @export
param_choices <- function(label, choices, default = NULL) {
  new_param("choices", label, default, choices = check_choices(choices))
}

#' @rdname param
#' @export
param_color <- function(label, default = NULL) {
  new_param("color", label, default)
}

#' @rdname param
#' @export
param_mapping <- function(label, keys, to = c("color", "text", "number"),
                          labels = NULL, default = NULL) {
  keys <- check_choices(keys, "keys")
  if (!is.null(labels) && (!is.character(labels) || length(labels) != length(keys))) {
    cli::cli_abort("{.arg labels} must be one string per key, or {.code NULL}.")
  }
  # `to` rather than `value_type`, so that `param$value` on an untouched
  # parameter cannot partial-match its way to the wrong answer.
  new_param("mapping", label, default, keys = keys, labels = labels,
            to = match.arg(to))
}

new_param <- function(type, label, default, ...) {
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    cli::cli_abort("{.arg label} must be a single non-empty string.")
  }
  param <- c(list(type = type, label = label, default = default), list(...))
  structure(param, class = "plots_param")
}

check_choices <- function(choices, arg = "choices", call = parent.frame()) {
  if (!is.character(choices) || !length(choices) || anyNA(choices)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector without {.val NA}.",
      call = call
    )
  }
  choices
}

# --- checking a value against the parameter that accepts it ------------------

# Returns the value, or stops. `NA` is always allowed: it means "no value",
# which a customizer reads as "take this away".
param_check <- function(param, value, key, call = parent.frame()) {
  if (is.null(value)) return(NULL)
  single_na <- length(value) == 1L && !is.list(value) && is.na(value)
  if (single_na) return(value)

  # `want` arrives already formatted: cli inserts it, it does not read it again.
  bad <- function(want) {
    cli::cli_abort(
      c("{.arg {key}} must be {want}.",
        "i" = "It is {.field {param$label}}, a {.emph {param$type}} parameter."),
      call = call
    )
  }

  switch(
    param$type,
    text = if (!is.character(value) || length(value) != 1L) bad("a single string"),
    number = {
      if (!is.numeric(value) || length(value) != 1L) bad("a single number")
      if (!is.null(param$min) && value < param$min) bad(cli::format_inline("at least {param$min}"))
      if (!is.null(param$max) && value > param$max) bad(cli::format_inline("at most {param$max}"))
    },
    flag = if (!is.logical(value) || length(value) != 1L) bad(cli::format_inline("{.code TRUE} or {.code FALSE}")),
    color = if (!is_color(value)) bad(cli::format_inline("a color, such as {.val #36c8ef}")),
    choice = {
      if (!is.character(value) || length(value) != 1L) bad("a single string")
      if (!value %in% param$choices) {
        bad(cli::format_inline("one of {.val {param$choices}}"))
      }
    },
    choices = {
      if (!is.character(value)) bad("a character vector")
      unknown <- setdiff(value, param$choices)
      if (length(unknown)) bad(cli::format_inline("chosen from {.val {param$choices}}"))
    },
    mapping = {
      value <- as.list(value)
      if (!length(names(value)) || anyNA(names(value)) || !all(nzchar(names(value)))) {
        bad("a named vector or list")
      }
      unknown <- setdiff(names(value), param$keys)
      if (length(unknown)) bad(cli::format_inline("named after {.val {param$keys}}"))
      ok <- switch(
        param$to,
        color = vapply(value, is_color, logical(1)),
        number = vapply(value, function(v) is.numeric(v) && length(v) == 1L, logical(1)),
        vapply(value, function(v) is.character(v) && length(v) == 1L, logical(1))
      )
      if (!all(ok)) bad(cli::format_inline("a mapping to {param$to} values"))
    },
    cli::cli_abort("Unknown parameter type {.val {param$type}}.", call = call)
  )
  value
}

is_color <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

# --- checks ------------------------------------------------------------------

check_customizer_fun <- function(fun, arg, call = parent.frame()) {
  if (!is.function(fun) || is.primitive(fun)) {
    cli::cli_abort("{.arg {arg}} must be a function.", call = call)
  }
  if (!length(formals(fun))) {
    cli::cli_abort("{.arg {arg}} must take the plot as its first argument.", call = call)
  }
  invisible(fun)
}

# A collection is read back in another session, so what it carries may not reach
# for anything that only exists in the session that wrote it. The collection's
# own functions travel with it, so their names are `known`.
check_travels <- function(fun, what, known = character(), call = parent.frame()) {
  risky <- travel_risks(fun, known)
  if (length(risky)) {
    cli::cli_abort(
      c(
        "{what} uses {length(risky)} name{?s} another session will not have: {.val {risky}}.",
        "i" = "A collection is read back in another session, where that is gone.",
        "i" = "Qualify a package function ({.code stringr::str_wrap()}), give a function to
               {.arg functions} in {.fn plots_init}, or pass a value in as a parameter."
      ),
      call = call
    )
  }
  invisible(fun)
}

# Packages a reader is sure to have attached, so their functions may go
# unqualified. forcats is among them: its factor helpers sit in plot mappings
# everywhere, such as `aes(y = fct_rev(brand))`.
travel_safe <- c("base", "stats", "utils", "graphics", "grDevices", "methods", "datasets",
                 "ggplot2", "forcats", "ggview")

# The names a function uses that another session will not have: those bound in
# this session, and those found only in an attached package a reader may not
# attach, shown as `str_wrap (stringr)`. A name bound nowhere is usually a data
# column (`aes(fill = brand)`, `filter(brand == ...)`), so it passes.
travel_risks <- function(fun, known = character()) {
  # codetools warns about `...` in closures it cannot place; that is not ours to report.
  globals <- suppressWarnings(codetools::findGlobals(fun, merge = FALSE))
  names <- setdiff(c(globals$variables, globals$functions), known)
  where <- vapply(names, binding_of, character(1), environment(fun))
  attached <- startsWith(where, "package:")
  package <- sub("^package:", "", where)
  risky <- where == "session" | (attached & !package %in% travel_safe)
  ifelse(where[risky] == "session", names[risky],
         paste0(names[risky], " (", package[risky], ")"))
}

# The functions a mapping calls that a reader will not have, as travel_risks() names them. A
# mapping is evaluated where the plot is drawn, so `aes(y = str_wrap(brand, 20))` needs
# stringr there. Only calls count: the other names in a mapping are data columns.
mapping_risks <- function(quo) {
  env <- rlang::quo_get_env(quo)
  fun <- rlang::new_function(list(), rlang::quo_get_expr(quo), env)
  calls <- suppressWarnings(codetools::findGlobals(fun, merge = FALSE))$functions
  where <- vapply(calls, binding_of, character(1), env)
  package <- sub("^package:", "", where)
  risky <- where == "session" | (startsWith(where, "package:") & !package %in% travel_safe)
  ifelse(where[risky] == "session", calls[risky], paste0(calls[risky], " (", package[risky], ")"))
}

# Where a name binds, starting from a function's own environment. "local" means
# it travels with the function, in a package namespace or in the function's own
# closure. "session" means it lives in the session that wrote the customizer and
# nowhere else. "package:<name>" means an attached package on the search path.
# "nowhere" means it cannot be found at all right now.
binding_of <- function(name, env) {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) {
      if (identical(env, globalenv())) return("session")
      label <- environmentName(env)
      return(if (startsWith(label, "package:")) label else "local")
    }
    env <- parent.env(env)
  }
  "nowhere"
}

check_customizer <- function(customizer, call = parent.frame()) {
  if (!inherits(customizer, "plots_customizer")) {
    cli::cli_abort(
      c(
        "{.arg customizer} must come from {.fn customizer}.",
        "i" = "{.fn customizer_default} is the one every plot starts with."
      ),
      call = call
    )
  }
  invisible(customizer)
}

# The parameters a customizer offers for one plot, checked on the way out. A
# later layer's parameter replaces an earlier one of the same name.
customizer_params <- function(customizer, plot, call = parent.frame()) {
  params <- list()
  for (layer in customizer$layers) {
    mine <- layer$params(plot)
    if (!is.list(mine) || (length(mine) && !length(names(mine)))) {
      cli::cli_abort("A customizer's {.arg params} must return a named list.", call = call)
    }
    wrong <- !vapply(mine, inherits, logical(1), "plots_param")
    if (any(wrong)) {
      cli::cli_abort(
        c("A customizer's {.arg params} must hold {.fn param_text} and friends.",
          "x" = "{.val {names(mine)[wrong]}} {?is/are} something else."),
        call = call
      )
    }
    params <- c(params[setdiff(names(params), names(mine))], mine)
  }
  check_reserved(names(params), call = call)
  params
}

# A customizer whose functions see the collection's functions, found before
# anything they captured themselves. The functions see each other the same way.
bind_functions <- function(customizer, functions) {
  if (!length(functions)) return(customizer)
  bind <- function(fun) {
    env <- list2env(functions, parent = environment(fun))
    for (name in names(functions)) environment(env[[name]]) <- env
    environment(fun) <- env
    fun
  }
  customizer$layers <- lapply(customizer$layers, function(layer) {
    layer$apply <- bind(layer$apply)
    layer$params <- bind(layer$params)
    layer
  })
  customizer
}

# The collection owns the canvas, so a customizer may not offer it.
check_reserved <- function(names, call = parent.frame()) {
  taken <- intersect(names, plots_reserved)
  if (length(taken)) {
    cli::cli_abort(
      c("A customizer may not offer {.val {taken}}.",
        "i" = "The collection owns the canvas: set {.arg width} and {.arg height} instead."),
      call = call
    )
  }
  invisible(names)
}

legend_position <- function(plot) theme_string(plot, "legend.position")

theme_string <- function(plot, name) {
  value <- plot$theme[[name]]
  if (is.character(value) && length(value) == 1L) value else NA_character_
}
