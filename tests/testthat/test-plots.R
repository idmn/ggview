p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

titled <- p + ggplot2::labs(title = "Title", subtitle = "Subtitle", caption = "Caption")

quietly <- function(expr) suppressMessages(expr)

using <- function(customizer) plots_init(customizers = list(default = customizer))

two_plots <- function() {
  quietly({
    plots <- plots_init(name = "Test bundle")
    plots <- plots_append(plots, titled, name = "Module 1/first", type = "bar", show = FALSE)
    plots_append(plots, p, name = "Module 1/second", type = "heatmap", show = FALSE)
  })
}

test_that("a new collection is empty and keeps what it was given", {
  plots <- plots_init(name = "Bundle", description = "One line")

  expect_s3_class(plots, "plots_tbl")
  expect_s3_class(plots, "tbl_df")
  expect_equal(nrow(plots), 0)
  expect_named(
    plots,
    c("id", "name", "type", "width", "height", "plot", "customizer", "theme", "values")
  )
  expect_equal(plots_meta(plots), list(name = "Bundle", description = "One line",
                                       path = NULL, plots = 0L, customizers = "default",
                                       functions = NULL, themes = 0L,
                                       packages = character()))

  wide <- plots_init(dims = plots_dims(heatmap = c(18, 11)))
  expect_equal(plots_meta_raw(wide)$dims$heatmap, c(18, 11))

  expect_error(plots_init(dims = list(bar = c(1, 1))), "plots_dims")
  expect_error(plots_init(customizers = list(a = "nope")), "must hold customizers")
  expect_error(plots_init(customizers = list(customizer_default())), "name of its own")
  expect_error(plots_init(functions = list(f = 1)), "must hold functions")
  expect_error(plots_init(name = c("a", "b")), "single string")
})

test_that("appending stores the plot as it is, and changes beside it", {
  plots <- quietly(plots_append(plots_init(), titled, name = "first",
                                title = "Mine", show = FALSE))

  expect_equal(nrow(plots), 1)
  expect_true(nzchar(plots$id))
  expect_equal(plots$values[[1]], list(title = "Mine"))
  # The plot itself is untouched.
  expect_equal(plots$plot[[1]]$labels$title, "Title")
  expect_equal(plots_pull(plots, 1)$labels$title, "Mine")

  bare <- quietly(plots_append(plots_init(), p, name = "bare", show = FALSE))
  expect_equal(bare$values[[1]], list())

  expect_true(anyDuplicated(two_plots()$id) == 0)
})

test_that("canvas size comes from the argument, then the plot, then the type", {
  by_type <- quietly(plots_append(plots_init(), p, name = "a", type = "heatmap", show = FALSE))
  expect_equal(c(by_type$width, by_type$height), c(16, 10))

  by_default <- quietly(plots_append(plots_init(), p, name = "a", type = "nope", show = FALSE))
  expect_equal(c(by_default$width, by_default$height), c(15, 9))

  by_canvas <- quietly(plots_append(plots_init(), p + canvas(7, 3), name = "a",
                                    type = "heatmap", show = FALSE))
  expect_equal(c(by_canvas$width, by_canvas$height), c(7, 3))

  by_arg <- quietly(plots_append(plots_init(), p + canvas(7, 3), name = "a", type = "heatmap",
                                 width = 4, height = 2, show = FALSE))
  expect_equal(c(by_arg$width, by_arg$height), c(4, 2))

  custom <- plots_init(dims = plots_dims(bar = c(3, 2)))
  custom <- quietly(plots_append(custom, p, name = "a", type = "bar", show = FALSE))
  expect_equal(c(custom$width, custom$height), c(3, 2))
})

test_that("appending a name twice updates that plot in place", {
  plots <- two_plots()
  id <- plots$id[[1]]

  expect_message(
    plots <- plots_append(plots, p, name = "Module 1/first", type = "line", show = FALSE),
    "Updated"
  )
  expect_equal(nrow(plots), 2)
  expect_equal(plots$id[[1]], id)
  expect_equal(plots$type[[1]], "line")
  expect_equal(plots$name[[2]], "Module 1/second")
})

test_that("bad input stops", {
  plots <- plots_init()

  expect_error(plots_append(mtcars, p, name = "a"), "plots_tbl")
  expect_error(plots_append(plots, mtcars, name = "a"), "ggplot")
  expect_error(plots_append(plots, p, name = ""), "non-empty")
  expect_error(plots_append(plots, p, name = "a", width = -1), "positive")
  expect_error(plots_append(plots, p, name = "a", nope = 1), "no parameter")
  expect_error(plots_dims(c(1, 2)), "named")
  expect_error(plots_dims(bar = 1), "two positive numbers")
  expect_error(plots_get(plots), "empty")
  expect_error(plots_get(two_plots(), "nope"), "No plot with name")
  expect_error(plots_get(two_plots(), name = "a", id = "b"), "not both")
})

test_that("getting a plot applies its changes and its canvas", {
  plots <- quietly(plots_append(plots_init(), titled, name = "first",
                                width = 6, height = 4, show = FALSE))
  plots <- plots_set(plots, "first", title = "New title", subtitle = NA)

  got <- plots_get(plots, "first")
  expect_equal(got$labels$title, "New title")
  expect_null(got$labels$subtitle)
  expect_equal(got$labels$caption, "Caption")
  expect_equal(got$canvas$width, 6)
  expect_equal(got$canvas$height, 4)
  expect_s3_class(got, "ggview")

  # Applying twice is applying once.
  expect_equal(plots_pull(plots, 1)$labels, plots_pull(plots, 1)$labels)

  plots <- two_plots()
  expect_equal(plots_get(plots)$labels$title, plots_get(plots, "Module 1/second")$labels$title)
  expect_equal(plots_get(plots, id = plots$id[[1]])$canvas$width, plots$width[[1]])
})

test_that("setting records a change and leaves the rest alone", {
  plots <- two_plots()

  changed <- plots_set(plots, "Module 1/first", title = "T", caption = "C",
                       legend = "bottom", width = 3, height = 2)
  expect_equal(changed$values[[1]]$title, "T")
  expect_equal(changed$values[[1]]$legend, "bottom")
  # The canvas is a change on top of the size the plot was added with.
  expect_equal(c(changed$values[[1]]$width, changed$values[[1]]$height), c(3, 2))
  expect_equal(c(changed$width[[1]], changed$height[[1]]), c(12, 8))
  expect_equal(plots_get(changed, "Module 1/first")$canvas$width, 3)
  expect_equal(changed$values[[2]], list())

  # A second change adds to the first rather than replacing it.
  twice <- plots_set(changed, "Module 1/first", subtitle = "S")
  expect_named(twice$values[[1]],
               c("title", "caption", "width", "height", "legend", "subtitle"))

  expect_error(plots_set(plots, "Module 1/first", color = "red"), "no parameter")
  expect_error(plots_set(plots, "Module 1/first", legend = "sideways"), "must be one of")
  expect_error(plots_set(plots, "Module 1/first", title = 1), "single string")
  expect_error(plots_set(plots, "nope", title = "T"), "No plot with name")
  expect_error(plots_set(plots, title = "T"), "name")
})

test_that("resetting forgets changes and restores the canvas", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "T", width = 3)

  one <- quietly(plots_reset(plots, "Module 1/first"))
  expect_equal(one$values[[1]], list())
  expect_equal(plots_pull(one, 1)$canvas$width, 12)
  expect_equal(plots_pull(one, 1)$labels$title, "Title")

  some <- quietly(plots_reset(plots, "Module 1/first", params = "width"))
  expect_equal(some$values[[1]]$title, "T")
  expect_equal(plots_pull(some, 1)$canvas$width, 12)

  all <- quietly(plots_reset(plots_set(plots, "Module 1/second", title = "S")))
  expect_equal(lengths(all$values), c(0L, 0L))

  # A size the script asked for is what a reset goes back to, not the type default.
  declared <- quietly(plots_append(plots_init(), p, name = "tall", type = "bar",
                                   height = 4.5, show = FALSE))
  declared <- quietly(plots_reset(plots_set(declared, "tall", height = 20), "tall"))
  expect_equal(plots_pull(declared, 1)$canvas$height, 4.5)

  expect_error(quietly(plots_reset(plots, "Module 1/first", params = "nope")),
               "Nothing to reset")
})

test_that("plots can be removed", {
  plots <- two_plots()

  one <- plots_remove(plots, "Module 1/first")
  expect_equal(nrow(one), 1)
  expect_equal(one$name, "Module 1/second")
  expect_s3_class(one, "plots_tbl")

  expect_equal(nrow(plots_remove(plots, id = plots$id)), 0)
  expect_error(plots_remove(plots, "nope"), "No plot with name")
})

test_that("params describe what a plot accepts", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "T")
  params <- plots_params(plots, "Module 1/first")

  expect_s3_class(params, "plots_params")
  expect_named(params, c("title", "title_size", "subtitle", "subtitle_size",
                         "caption", "caption_size", "x", "x_size", "y", "y_size",
                         "axis_text_size", "axis_text_wrap", "text_size", "legend",
                         "legend_direction", "x_grid", "y_grid", "width", "height"))
  expect_equal(params$title$type, "text")
  expect_equal(params$title$default, "Title")
  expect_equal(params$title$value, "T")
  expect_null(params$subtitle$value)
  expect_equal(params$legend$choices, c("right", "left", "top", "bottom", "none"))

  # The canvas is offered by the collection, not the customizer.
  expect_equal(params$width$type, "number")
  expect_equal(params$width$default, 12)
  expect_null(params$width$value)
  expect_equal(plots_params(plots_set(plots, "Module 1/first", width = 4),
                            "Module 1/first")$width$value, 4)
})

test_that("the canvas survives whatever unit it arrived in", {
  px <- quietly(plots_append(plots_init(), p + canvas(1200, 900, units = "px", dpi = 300),
                             name = "a", show = FALSE))
  expect_equal(c(px$width, px$height), c(4, 3))

  cm <- quietly(plots_append(plots_init(), p + canvas(2.54, 5.08, units = "cm"),
                             name = "a", show = FALSE))
  expect_equal(c(cm$width, cm$height), c(1, 2))
})

test_that("a canvas that is not usable stops at render", {
  plots <- two_plots()
  plots$width[[1]] <- NA_real_
  expect_error(plots_pull(plots, 1), "not usable")

  plots$width[[1]] <- -3
  expect_error(plots_pull(plots, 1), "not usable")
})

test_that("a name may not have an empty folder", {
  for (bad in c("a/", "/a", "a//b")) {
    expect_error(quietly(plots_append(plots_init(), p, name = bad, show = FALSE)),
                 "empty folder")
  }
})

test_that("what a collection carries must be able to travel", {
  expect_error(customizer("nope", function(plot) list()), "must be a function")
  expect_error(customizer(function() NULL, function(plot) list()), "first argument")
  carries <- function(apply) {
    plots_init(customizers = list(x = customizer(apply, function(plot) list())))
  }

  # Bound nowhere: usually a data column, as in `aes(fill = brand)`, so it is allowed.
  column_ref <- function(plot, a = NULL) plot + ggplot2::aes(fill = brand)
  environment(column_ref) <- globalenv()
  expect_s3_class(carries(column_ref), "plots_tbl")

  # Found only in the session that wrote it: the same problem, later.
  assign("a_session_object", "gone tomorrow", envir = globalenv())
  on.exit(rm("a_session_object", envir = globalenv()), add = TRUE)
  session_var <- function(plot, a = NULL) paste(plot, a_session_object)
  environment(session_var) <- globalenv()
  expect_error(carries(session_var), "another session will not have")

  # A function it does not define is the same problem: it would not be found.
  assign("a_project_builder", function(x) x, envir = globalenv())
  on.exit(rm("a_project_builder", envir = globalenv()), add = TRUE)
  calls_global <- function(plot, a = NULL) a_project_builder(plot)
  environment(calls_global) <- globalenv()
  expect_error(carries(calls_global), "a_project_builder")

  # Unless the collection carries it too.
  expect_s3_class(
    plots_init(customizers = list(x = customizer(calls_global, function(plot) list())),
               functions = list(a_project_builder = function(x) x)),
    "plots_tbl"
  )
  # And a function it carries is checked the same way.
  helper <- function(x) a_project_builder(x)
  environment(helper) <- globalenv()
  expect_error(plots_init(functions = list(helper = helper)), "Function .helper.")

  # Calling a package's function, either way round, is not.
  qualified <- function(plot, a = NULL) ggplot2::labs(title = a)
  bare <- function(plot, a = NULL) labs(title = a)
  environment(qualified) <- globalenv()
  environment(bare) <- globalenv()
  expect_s3_class(carries(qualified), "plots_tbl")
  expect_s3_class(carries(bare), "plots_tbl")

  # A package's own objects, and things the function holds itself, are fine.
  held <- local({
    inner <- "kept"
    function(plot, a = NULL) paste(plot, inner)
  })
  expect_s3_class(carries(held), "plots_tbl")
})

test_that("customizers call the collection's functions", {
  shout <- customizer_extend(
    customizer_default(),
    apply = function(plot, shout = NULL) {
      if (is.null(shout)) plot else plot + ggplot2::labs(title = loud(shout))
    },
    params = function(plot) list(shout = param_text("Shout", default = loud("x")))
  )
  plots <- plots_init(
    customizers = list(shout = shout),
    # The functions see each other.
    functions = list(loud = function(x) paste0(upper(x), "!"), upper = toupper)
  )
  expect_error(plots_append(plots, p, name = "a", customizer = "nope", show = FALSE),
               "must name one of")

  plots <- quietly(plots_append(plots, p, name = "a", customizer = "shout",
                                shout = "hello", show = FALSE))
  expect_equal(plots$customizer, "shout")
  expect_equal(plots_params(plots, "a")$shout$default, "X!")
  expect_equal(plots_pull(plots, 1)$labels$title, "HELLO!")
  expect_equal(plots_meta(plots)$customizers, c("default", "shout"))

  # And still do once the collection has been saved and read back.
  file <- tempfile(fileext = ".rds")
  saveRDS(plots, file)
  back <- readRDS(file)
  expect_equal(plots_pull(plots_set(back, "a", shout = "again", show = FALSE), 1)$labels$title,
               "AGAIN!")
})

test_that("a stored customizer carries no source records", {
  # A source record can drag the whole source of the package that made the
  # function into every saved collection.
  with_source <- eval(parse(text = "function(plot, note = NULL) plot", keep.source = TRUE))
  expect_false(is.null(attr(with_source, "srcref")))
  params <- function(plot) list(note = param_text("Note"))
  # Away from the test's own plots, so the size below is the customizer's alone.
  environment(with_source) <- environment(params) <- globalenv()
  extended <- customizer_extend(customizer_default(), apply = with_source, params = params)
  for (layer in extended$layers) {
    expect_null(attr(layer$apply, "srcref"))
    expect_null(attr(layer$params, "srcref"))
  }
  expect_lt(length(serialize(extended, NULL)), 200000)
})

test_that("a customizer may not take over the canvas", {
  expect_error(
    customizer(function(plot, width = NULL) plot, function(plot) list()),
    "may not offer"
  )

  # And the same check again where the parameters are actually built.
  sneaky <- customizer(
    apply = function(plot, ...) plot,
    params = function(plot) list(width = param_number("Width"))
  )
  plots <- quietly(plots_append(using(sneaky), p, name = "a", show = FALSE))
  expect_error(plots_params(plots, "a"), "may not offer")

  wrong <- customizer(
    apply = function(plot, a = NULL) plot,
    params = function(plot) list(a = "not a parameter")
  )
  plots <- quietly(plots_append(using(wrong), p, name = "a", show = FALSE))
  expect_error(plots_params(plots, "a"), "param_text")
})

test_that("a custom customizer runs, and is checked", {
  recolor <- customizer(
    apply = function(plot, colors = NULL) {
      if (is.null(colors)) return(plot)
      suppressMessages(plot + ggplot2::scale_color_manual(values = unlist(colors)))
    },
    params = function(plot) {
      list(colors = param_mapping("Colors", keys = c("4", "6", "8"), to = "color"))
    }
  )
  dots <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()

  plots <- quietly(plots_append(using(recolor), dots, name = "dots", show = FALSE))
  expect_equal(names(plots_params(plots, "dots")), c("colors", "width", "height"))

  plots <- plots_set(plots, "dots", colors = c("4" = "#36c8ef"))
  expect_s3_class(plots_pull(plots, 1), "ggplot")

  expect_error(plots_set(plots, "dots", colors = c("9" = "#36c8ef")), "named after")
  expect_error(plots_set(plots, "dots", colors = c("4" = "not a color")), "mapping to color")

  # A customizer that returns something else is caught at render time.
  broken <- customizer(function(plot, a = NULL) "not a plot", function(plot) {
    list(a = param_text("A"))
  })
  oops <- quietly(plots_append(using(broken), p, name = "x", a = "go", show = FALSE))
  expect_error(plots_pull(oops, 1), "did not return a plot")
})

test_that("the default reaches sizes, gridlines and the legend", {
  effective <- function(plot, element) {
    ggplot2::calc_element(element, ggplot2::complete_theme(plot$theme))
  }
  plots <- quietly(plots_append(plots_init(), titled, name = "a", type = "bar",
                                show = FALSE))

  # Defaults are read off the plot, not left empty.
  params <- plots_params(plots, "a")
  expect_true(is.numeric(params$title_size$default))
  expect_true(params$x_grid$default)

  changed <- plots_set(plots, "a", title_size = 30, caption_size = 12, text_size = 16,
                       legend = "bottom", legend_direction = "horizontal",
                       x_grid = FALSE, y_grid = TRUE, show = FALSE)
  out <- plots_pull(changed, 1)

  expect_equal(effective(out, "plot.title")$size, 30)
  expect_equal(effective(out, "plot.caption")$size, 12)
  expect_equal(effective(out, "text")$size, 16)
  expect_equal(effective(out, "legend.position"), "bottom")
  expect_equal(effective(out, "legend.direction"), "horizontal")
  expect_s3_class(effective(out, "panel.grid.major.x"), "element_blank")
  expect_false(inherits(effective(out, "panel.grid.major.y"), "element_blank"))

  # The stored plot is untouched, so a reset gives the original size back.
  expect_equal(effective(plots_pull(quietly(plots_reset(changed, "a")), 1), "plot.title")$size,
               effective(plots_pull(plots, 1), "plot.title")$size)

  expect_error(plots_set(plots, "a", legend_direction = "sideways"), "must be one of")
  expect_error(plots_set(plots, "a", x_grid = "yes"), "TRUE")
})

test_that("axis text can be resized and wrapped", {
  answers <- data.frame(k = c("Very satisfied overall", "Not at all"), v = 1:2)
  bars <- ggplot2::ggplot(answers, ggplot2::aes(k, v)) + ggplot2::geom_col()
  plots <- quietly(plots_append(plots_init(), bars, name = "a", show = FALSE))

  out <- plots_pull(plots_set(plots, "a", axis_text_size = 18, axis_text_wrap = 12,
                              show = FALSE), 1)
  theme <- ggplot2::complete_theme(out$theme)
  expect_equal(ggplot2::calc_element("axis.text.x", theme)$size, 18)
  expect_equal(ggplot2::calc_element("axis.text.y", theme)$size, 18)

  labels <- suppressMessages(
    ggplot2::ggplot_build(out)$layout$panel_params[[1]]$x$get_labels()
  )
  expect_true(any(grepl("\n", labels, fixed = TRUE)))

  # A continuous axis has no labels to wrap, and must not be broken by trying.
  points <- quietly(plots_append(plots_init(),
                                 ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
                                   ggplot2::geom_point(),
                                 name = "b", show = FALSE))
  expect_s3_class(plots_pull(plots_set(points, "b", axis_text_wrap = 10, show = FALSE), 1),
                  "ggplot")
})

test_that("a size change keeps the kind of element a title is drawn with", {
  skip_if_not_installed("ggtext")
  boxed <- titled + ggplot2::theme(
    plot.title = ggtext::element_textbox_simple(size = 14)
  )
  plots <- quietly(plots_append(plots_init(), boxed, name = "a", show = FALSE))
  out <- plots_pull(plots_set(plots, "a", title_size = 30, show = FALSE), 1)

  # Replacing it outright is an error in ggplot2, so it must be changed in place.
  element <- ggplot2::calc_element("plot.title", ggplot2::complete_theme(out$theme))
  expect_s3_class(element, "element_textbox")
  expect_equal(element$size, 30)
})

test_that("a customizer can be extended rather than replaced", {
  extended <- customizer_extend(
    customizer_default(),
    apply = function(plot, note = NULL) {
      if (is.null(note)) plot else plot + ggplot2::labs(tag = note)
    },
    params = function(plot) list(note = param_text("Note"))
  )
  plots <- quietly(plots_append(using(extended), titled, name = "a", show = FALSE))

  expect_true(all(c("title", "note") %in% names(plots_params(plots, "a"))))

  both <- plots_set(plots, "a", title = "New", note = "Draft")
  rendered <- plots_pull(both, 1)
  expect_equal(rendered$labels$title, "New")
  expect_equal(rendered$labels$tag, "Draft")
})

test_that("a mapping arrives whole, and can be labelled", {
  keyed <- customizer(
    apply = function(plot, colors = NULL) {
      if (!is.null(colors)) attr(plot, "seen") <- colors
      plot
    },
    params = function(plot) {
      list(colors = param_mapping("Colors", keys = c("a", "b"), to = "color",
                                  labels = c("Ours", "Theirs"),
                                  default = list(a = "#111111", b = "#222222")))
    }
  )
  plots <- quietly(plots_append(using(keyed), p, name = "x", show = FALSE))

  # One key changed, the rest kept.
  one <- plots_set(plots, "x", colors = c(a = "#36c8ef"))
  expect_equal(one$values[[1]]$colors, list(a = "#36c8ef", b = "#222222"))

  # And a second change builds on the first.
  two <- plots_set(one, "x", colors = c(b = "#50bd90"))
  expect_equal(two$values[[1]]$colors, list(a = "#36c8ef", b = "#50bd90"))

  expect_equal(plots_params(plots, "x")$colors$labels, c("Ours", "Theirs"))
  expect_error(param_mapping("C", keys = c("a", "b"), labels = "only one"),
               "one string per key")
})

test_that("each kind of parameter checks its value", {
  expect_error(param_check(param_text("T"), 1, "k"), "single string")
  expect_error(param_check(param_number("N", min = 2), 1, "k"), "at least")
  expect_error(param_check(param_number("N", max = 2), 3, "k"), "at most")
  expect_error(param_check(param_flag("F"), "yes", "k"), "TRUE")
  expect_error(param_check(param_color("C"), "nope", "k"), "color")
  expect_error(param_check(param_choice("C", c("a")), "b", "k"), "one of")
  expect_error(param_check(param_choices("C", c("a")), c("a", "b"), "k"), "chosen from")
  expect_error(param_check(param_mapping("M", "a"), "b", "k"), "named vector")

  expect_equal(param_check(param_text("T"), NA, "k"), NA)
  expect_null(param_check(param_text("T"), NULL, "k"))
  expect_equal(param_check(param_color("C"), "#36c8ef", "k"), "#36c8ef")
  expect_equal(param_check(param_color("C"), "red", "k"), "red")
  expect_error(param_number("N", min = "low"), "single number")
  expect_error(param_choice("C", 1), "character vector")
})

test_that("dplyr verbs keep the class and the collection's details", {
  skip_if_not_installed("dplyr")
  plots <- two_plots()

  bigger <- dplyr::mutate(plots, width = ifelse(type == "heatmap", 20, width))
  expect_s3_class(bigger, "plots_tbl")
  expect_equal(plots_meta(bigger)$name, "Test bundle")
  expect_equal(bigger$width[[2]], 20)
  expect_equal(plots_get(bigger, "Module 1/second")$canvas$width, 20)

  expect_s3_class(dplyr::filter(plots, type == "bar"), "plots_tbl")
  expect_s3_class(dplyr::arrange(plots, name), "plots_tbl")
  expect_s3_class(plots[1, ], "plots_tbl")
})

test_that("a collection turns into content and files", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed")

  content <- plots_as_content(plots, path = "plots")
  expect_named(content, c("object", "name", "type"))
  expect_s3_class(content$object[[1]], "ggview")
  expect_equal(content$object[[1]]$labels$title, "Changed")
  expect_equal(content$name[[1]], "plots/Module 1/first.png")
  expect_equal(names(content$name), unname(content$name))

  dir <- tempfile()
  files <- save_plots(plots, dir = dir, width = 4, height = 3)
  expect_equal(basename(files), c("first.png", "second.png"))
  expect_true(all(file.exists(files)))
  expect_equal(basename(dirname(files[[1]])), "Module 1")

  expect_error(save_plots(plots_init(), dir = dir), "empty")
  expect_error(plots_as_content(plots_init()), "empty")
})

test_that("a collection browses as a gallery", {
  plots <- quietly(plots_append(two_plots(), p, name = "loose", show = FALSE))

  gal <- as.gallery(plots)
  expect_s3_class(gal, "gallery")
  expect_named(gal, c("Module 1", "loose"))
  expect_named(gal[["Module 1"]], c("first", "second"))
  expect_s3_class(gal[["Module 1"]][["first"]], "ggview")

  clash <- quietly(plots_append(two_plots(), p, name = "Module 1", show = FALSE))
  expect_error(as.gallery(clash), "both a plot and a folder")
})

test_that("as_tibble gives the plain tibble", {
  plain <- tibble::as_tibble(two_plots())

  expect_s3_class(plain, "tbl_df")
  expect_false(inherits(plain, "plots_tbl"))
  expect_null(attr(plain, "meta"))
  expect_equal(nrow(plain), 2)
})

test_that("a collection survives a save and a read", {
  file <- tempfile(fileext = ".rds")
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", width = 4)
  saveRDS(plots, file)
  back <- readRDS(file)

  expect_s3_class(back, "plots_tbl")
  expect_equal(back$id, plots$id)
  expect_equal(plots_meta(back)$name, "Test bundle")
  expect_equal(plots_pull(back, 1)$labels$title, "Changed")
  expect_equal(plots_params(back, "Module 1/first")$width$value, 4)
  expect_equal(plots_pull(quietly(plots_reset(back)), 1)$labels$title, "Title")
})

test_that("a plot prints without a viewer instead of stopping", {
  # Knitting, a plain console and Rscript all have no viewer. The canvas cannot
  # be honored there, but the plot must still draw.
  file <- tempfile(fileext = ".png")
  grDevices::png(file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(print(p + canvas(8, 5)))
  expect_no_error(print(plots_get(two_plots(), "Module 1/first")))
})

test_that("a printed collection can be shown by its links", {
  the$printed <- NULL
  expect_error(plots_show("Module 1/first"), "No collection")

  plots <- two_plots()
  print(plots)
  expect_equal(the$printed$id, plots$id)
  expect_error(plots_show("nope"), "printed last")

  old <- options(cli.hyperlink = TRUE, cli.hyperlink_run = TRUE)
  on.exit(options(old), add = TRUE)

  # The IDE echoes what the link runs, so it runs the name, not the id.
  linked <- plots_link("first", "Module 1/first", 10)
  expect_true(grepl('plots_show("Module 1/first")', linked, fixed = TRUE))
  expect_equal(cli::ansi_nchar(linked), 10)
  expect_true(grepl('"a \\"b\\""', plots_link("a", 'a "b"', 5), fixed = TRUE))

  options(cli.hyperlink = FALSE, cli.hyperlink_run = FALSE)
  expect_equal(plots_link("first", "Module 1/first", 10), "first     ")
})

test_that("printing lists the collection and its parameters", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", width = 4)
  expect_snapshot({
    print(plots_init())
    print(plots)
    print(plots_params(plots, "Module 1/first"))
    print(plots_dims(heatmap = c(18, 11)))
    print(customizer_default())
  })
})

test_that("an extension of an extension still reaches the base parameters", {
  one <- customizer_extend(customizer_default(),
                           apply = function(plot, note = NULL) {
                             if (is.null(note)) plot else plot + ggplot2::labs(tag = note)
                           },
                           params = function(plot) list(note = param_text("Note")))
  two <- customizer_extend(one,
                           apply = function(plot, title = NULL, alt = NULL) {
                             if (is.null(title)) return(plot)
                             plot + ggplot2::labs(title = toupper(title))
                           },
                           params = function(plot) list(title = param_text("Title"),
                                                        alt = param_text("Alt")))
  expect_true(all(c("title", "subtitle", "note", "alt") %in% customizer_keys(two)))

  plots <- quietly(plots_append(using(two), titled, name = "a", show = FALSE))
  out <- plots_pull(plots_set(plots, "a", title = "new", subtitle = "Sub", note = "N",
                              show = FALSE), 1)
  expect_equal(out$labels$title, "NEW")      # the added parameter shadows the base one
  expect_equal(out$labels$subtitle, "Sub")   # the base of the base still applies
  expect_equal(out$labels$tag, "N")
})

test_that("the collection keeps a plot's resolution and background", {
  plots <- quietly(plots_append(plots_init(),
                                p + canvas(5, 4, dpi = 150, bg = "transparent", scale = 2),
                                name = "a", show = FALSE))
  out <- plots_pull(plots_set(plots, "a", width = 8, show = FALSE), 1)
  expect_equal(out$canvas$width, 8)
  expect_equal(out$canvas$dpi, 150)
  expect_equal(out$canvas$bg, "transparent")
  expect_equal(out$canvas$scale, 2)
})

test_that("wrapping keeps what the plot's own scale set", {
  answers <- data.frame(k = c("very satisfied overall", "not at all"), v = 1:2)
  bars <- ggplot2::ggplot(answers, ggplot2::aes(k, v)) + ggplot2::geom_col() +
    ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = 2),
                              labels = toupper)
  plots <- quietly(plots_append(plots_init(), bars, name = "a", show = FALSE))
  out <- plots_pull(plots_set(plots, "a", axis_text_wrap = 10, show = FALSE), 1)
  scale <- out$scales$get_scales("x")
  expect_equal(scale$position, "top")
  expect_false(inherits(scale$expand, "waiver"))
  labels <- scale$labels(c("very satisfied overall"))
  expect_true(grepl("\n", labels, fixed = TRUE))
  expect_equal(labels, toupper(labels))
})

test_that("showing a grid brings back only its major lines", {
  plain <- p + ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())
  plots <- quietly(plots_append(plots_init(), plain, name = "a", y_grid = TRUE, show = FALSE))
  theme <- ggplot2::complete_theme(plots_pull(plots, 1)$theme)
  expect_false(inherits(ggplot2::calc_element("panel.grid.major.y", theme), "element_blank"))
  expect_s3_class(ggplot2::calc_element("panel.grid.minor.y", theme), "element_blank")
})

test_that("the overall text size reaches elements with their own size", {
  sized <- titled + ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                                   plot.title = ggplot2::element_text(size = 20),
                                   text = ggplot2::element_text(size = 10))
  plots <- quietly(plots_append(plots_init(), sized, name = "a", show = FALSE))
  size_of <- function(plot, element) {
    ggplot2::calc_element(element, ggplot2::complete_theme(plot$theme))$size
  }
  out <- plots_pull(plots_set(plots, "a", text_size = 20, show = FALSE), 1)
  expect_equal(size_of(out, "plot.title"), 40)
  expect_equal(size_of(out, "axis.text"), 20)

  # A specific size given with it wins.
  out <- plots_pull(plots_set(plots, "a", text_size = 20, title_size = 25, show = FALSE), 1)
  expect_equal(size_of(out, "plot.title"), 25)
})

test_that("adding a plot again keeps the changes recorded against it", {
  plots <- quietly(plots_append(plots_init(), titled, name = "a", show = FALSE))
  plots <- plots_set(plots, "a", title = "Kept", width = 7, show = FALSE)
  again <- quietly(plots_append(plots, titled, name = "a", show = FALSE))
  expect_equal(plots_pull(again, 1)$labels$title, "Kept")
  expect_equal(plots_pull(again, 1)$canvas$width, 7)

  # `...` replaces a change of the same name.
  again <- quietly(plots_append(plots, titled, name = "a", title = "New", show = FALSE))
  expect_equal(plots_pull(again, 1)$labels$title, "New")

  # A change the new plot's customizer no longer offers is dropped, with a warning.
  bare <- customizer(function(plot, other = NULL) plot,
                     function(plot) list(other = param_text("Other")))
  plots <- plots_init(customizers = list(bare = bare))
  plots <- quietly(plots_append(plots, titled, name = "a", show = FALSE))
  plots <- plots_set(plots, "a", title = "Kept", show = FALSE)
  expect_message(plots_append(plots, titled, name = "a", customizer = "bare", show = FALSE),
                 "Dropped 1 change")
})

test_that("a failing customizer names the plot", {
  boom <- customizer(function(plot, z = NULL) stop("boom"),
                     function(plot) list(z = param_text("Z")))
  plots <- quietly(plots_append(using(boom), p, name = "Module/bad", z = "go",
                                show = FALSE))
  expect_error(plots_pull(plots, 1), "Module/bad")
})

test_that("the default offers a color per category and keeps the legend", {
  values <- c("4" = "#36c8ef", "6" = "grey75", "8" = "#de425b")
  bars <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl),
                                                colour = factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(name = "", values = values) +
    ggplot2::scale_colour_manual(name = "", values = values, guide = "none")
  plots <- quietly(plots_append(plots_init(), bars, name = "a", show = FALSE))

  colors <- plots_params(plots, "a")$colors
  expect_equal(colors$keys, c("4", "6", "8"))
  # Hex, as a color picker shows it, however the plot spelled the color.
  expect_equal(unlist(colors$default), c("4" = "#36C8EF", "6" = "#BFBFBF", "8" = "#DE425B"))

  out <- plots_pull(plots_set(plots, "a", colors = c("6" = "#50BD90"), show = FALSE), 1)
  drawn <- ggplot2::layer_data(out)
  expect_equal(toupper(unique(drawn$fill)), c("#36C8EF", "#50BD90", "#DE425B"))
  # The colour scale holds the same categories, so it follows the fill.
  expect_equal(toupper(unique(drawn$colour)), c("#36C8EF", "#50BD90", "#DE425B"))
  # Changed in place: no legend title appears and the colour legend stays off.
  expect_identical(out$scales$get_scales("fill")$name, "")
  expect_identical(out$scales$get_scales("colour")$guide, "none")

  # A plot with no scale of its own keeps the colors it did not change.
  hue <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl))) +
    ggplot2::geom_bar()
  plots <- quietly(plots_append(plots_init(), hue, name = "h", show = FALSE))
  before <- unlist(plots_params(plots, "h")$colors$default)
  out <- plots_pull(plots_set(plots, "h", colors = c("8" = "#123456"), show = FALSE), 1)
  expect_equal(toupper(unique(ggplot2::layer_data(out)$fill)),
               unname(c(before[c("4", "6")], "#123456")))
})

test_that("identity and continuous color scales are handled", {
  ided <- ggplot2::ggplot(data.frame(x = c("a", "b"), y = 1:2, col = c("#36c8ef", "grey")),
                          ggplot2::aes(x, y, fill = col)) +
    ggplot2::geom_col() + ggplot2::scale_fill_identity()
  plots <- quietly(plots_append(plots_init(), ided, name = "i", show = FALSE))
  # Never trained without a legend, so the categories are the colors drawn.
  expect_equal(plots_params(plots, "i")$colors$keys, c("#36c8ef", "grey"))
  out <- plots_pull(plots_set(plots, "i", colors = c(grey = "#50BD90"), show = FALSE), 1)
  expect_equal(toupper(ggplot2::layer_data(out)$fill), c("#36C8EF", "#50BD90"))

  tiles <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2, v = 1:2), ggplot2::aes(x, y, fill = v)) +
    ggplot2::geom_tile()
  plots <- quietly(plots_append(plots_init(), tiles, name = "t", show = FALSE))
  expect_null(plots_params(plots, "t")$colors)
  expect_error(plots_set(plots, "t", colors = c(a = "#ff0000")), "no parameter")
})

test_that("a grid the plot hid comes back where it can be seen", {
  grid_of <- function(plot, axis) {
    ggplot2::calc_element(paste0("panel.grid.major.", axis), ggplot2::complete_theme(plot$theme))
  }
  # The parent gridline is white on a white panel, as in a house theme.
  flat <- p + ggplot2::theme_minimal() + ggplot2::theme(
    panel.grid = ggplot2::element_line(colour = "white"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#e9ecef")
  )
  plots <- quietly(plots_append(plots_init(), flat, name = "g", show = FALSE))
  expect_false(plots_params(plots, "g")$x_grid$default)
  shown <- plots_pull(plots_set(plots, "g", x_grid = TRUE, show = FALSE), 1)
  expect_equal(grid_of(shown, "x")$colour, "#e9ecef")   # drawn like the other axis

  bare <- flat + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  plots <- quietly(plots_append(plots_init(), bare, name = "b", show = FALSE))
  expect_false(plots_params(plots, "b")$y_grid$default)
  shown <- plots_pull(plots_set(plots, "b", y_grid = TRUE, show = FALSE), 1)
  expect_equal(grid_of(shown, "y")$colour, "grey92")   # nothing visible to copy
})

test_that("a function from an attached package must be qualified", {
  attach(list(fake_wrap = function(x) x), name = "package:fakewrap", warn.conflicts = FALSE)
  on.exit(detach("package:fakewrap"), add = TRUE)
  carries <- function(apply) {
    environment(apply) <- globalenv()
    plots_init(customizers = list(x = customizer(apply, function(plot) list())))
  }

  # Attached here, and nowhere a reader promises to attach it.
  expect_error(carries(function(plot, a = NULL) fake_wrap(plot)), "fake_wrap \\(fakewrap\\)")
  # base R, ggplot2 and ggview are safe unqualified; anything qualified is safe.
  expect_s3_class(carries(function(plot, a = NULL) plot + labs(title = paste(a))), "plots_tbl")
  expect_s3_class(carries(function(plot, a = NULL) fakewrap::fake_wrap(plot)), "plots_tbl")
})

test_that("a plot carrying a function that will not travel is added with a warning", {
  attach(list(fake_wrap = function(x) x), name = "package:fakewrap", warn.conflicts = FALSE)
  on.exit(detach("package:fakewrap"), add = TRUE)
  labeller <- function(x) fake_wrap(x)
  environment(labeller) <- globalenv()
  wrapped <- p + ggplot2::scale_x_continuous(labels = labeller)
  expect_warning(
    quietly(plots_append(plots_init(), wrapped, name = "Module/wrapped", show = FALSE)),
    "Module/wrapped"
  )
  # A scales helper travels.
  expect_no_warning(
    quietly(plots_append(plots_init(), p + ggplot2::scale_x_continuous(labels = scales::label_comma()),
                         name = "fine", show = FALSE))
  )
})

test_that("plots that share a theme store one copy of it", {
  themed <- titled + ggplot2::theme_minimal()
  plots <- quietly({
    plots <- plots_init()
    plots <- plots_append(plots, themed, name = "a", show = FALSE)
    plots <- plots_append(plots, themed + ggplot2::labs(title = "Other"), name = "b", show = FALSE)
    plots_append(plots, titled, name = "c", show = FALSE)
  })
  expect_equal(plots_meta(plots)$themes, 2L)
  expect_equal(plots$theme[[1]], plots$theme[[2]])
  # The stored plot carries no theme; the plot that comes out has all of it.
  expect_length(plots$plot[[1]]$theme, 0)
  out <- plots_pull(plots, 1)
  expect_equal(ggplot2::calc_element("panel.grid.major.x", ggplot2::complete_theme(out$theme)),
               ggplot2::calc_element("panel.grid.major.x", ggplot2::complete_theme(themed$theme)))
  # A customizer reads the theme as the plot had it.
  expect_equal(plots_params(plots, "a")$title_size$default,
               ggplot2::calc_element("plot.title", ggplot2::complete_theme(themed$theme))$size)

  # Two copies cost little more than one.
  one <- quietly(plots_append(plots_init(), themed, name = "a", show = FALSE))
  two <- quietly(plots_append(one, themed, name = "b", show = FALSE))
  expect_lt(length(serialize(two, NULL)), 1.5 * length(serialize(one, NULL)))

  # A filtered collection keeps only the themes its plots use.
  expect_equal(plots_meta(plots[plots$name == "c", ])$themes, 1L)
})

test_that("a theme drawn by another package records that package", {
  skip_if_not_installed("ggtext")
  boxed <- p + ggplot2::theme(plot.title = ggtext::element_textbox_simple())
  plots <- quietly(plots_append(plots_init(), boxed, name = "a", show = FALSE))
  expect_equal(plots_meta(plots)$packages, "ggtext")
})

test_that("a summary of a collection is a plain tibble", {
  plots <- two_plots()
  counted <- dplyr::count(plots, type)
  expect_false(inherits(counted, "plots_tbl"))
  expect_no_error(print(counted))
  expect_false(inherits(plots[, c("name", "type")], "plots_tbl"))
  expect_s3_class(dplyr::filter(plots, type == "bar"), "plots_tbl")
})

test_that("the reports can be silenced", {
  withr::local_options(ggview.quiet = TRUE)
  expect_no_message(plots <- plots_append(plots_init(), titled, name = "a", show = FALSE))
  expect_no_message(plots_get(plots, "a"))
  expect_no_message(plots_reset(plots, show = FALSE))
})

test_that("a caption may run over several lines", {
  two_lines <- p + ggplot2::labs(caption = "Base: all\nSource: survey")
  plots <- quietly(plots_append(plots_init(), two_lines, name = "a", show = FALSE))
  params <- plots_params(plots, "a")
  expect_true(params$caption$multiline)
  expect_true(params$subtitle$multiline)
  expect_false(params$title$multiline)
  expect_error(param_text("T", multiline = "yes"), "TRUE")
  # The listing keeps the caption on one line.
  shown <- cli::ansi_strip(utils::capture.output(print(params), type = "message"))
  expect_false(any(grepl("^Source: survey", shown)))
})

test_that("a collection keeps its path, and plots_as_content() writes there", {
  plots <- quietly(plots_append(plots_init(path = "results/plots"), p, name = "A/one",
                                show = FALSE))
  expect_equal(plots_meta(plots)$path, "results/plots")
  expect_equal(unname(plots_as_content(plots)$name), "results/plots/A/one.png")
  # A path given here is for this write only.
  expect_equal(unname(plots_as_content(plots, path = "drafts")$name), "drafts/A/one.png")
  expect_equal(plots_meta(plots)$path, "results/plots")

  # The path goes wherever the collection goes.
  expect_equal(plots_meta(plots[1, ])$path, "results/plots")
  expect_equal(plots_meta(quietly(plots_append(plots, p, name = "A/two", show = FALSE)))$path,
               "results/plots")
  file <- tempfile(fileext = ".rds")
  saveRDS(plots, file)
  expect_equal(plots_meta(readRDS(file))$path, "results/plots")
  skip_if_not_installed("dplyr")
  expect_equal(plots_meta(dplyr::filter(plots, name == "A/one"))$path, "results/plots")
})

test_that("plots_set_path() moves the collection and nothing else", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", show = FALSE)
  moved <- plots_set_path(plots, "results/2026-09/plots")

  expect_equal(plots_meta(moved)$path, "results/2026-09/plots")
  expect_equal(moved$values, plots$values)
  expect_equal(moved$id, plots$id)
  expect_null(plots_meta(plots_set_path(moved, NULL))$path)
  expect_error(plots_as_content(plots_set_path(moved, NULL)), "no path")

  for (bad in c("/results", "results/", "a//b", "../up", "a/./b", "a\\b")) {
    expect_error(plots_init(path = bad), "relative folder")
  }
  expect_error(plots_init(path = ""), "non-empty")
  expect_error(plots_set_path(moved, c("a", "b")), "non-empty")
  expect_error(plots_set_path(mtcars, "a"), "plots_tbl")
})

test_that("plots_changes() lists what differs from the defaults", {
  dots <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() + ggplot2::labs(title = "Dots")
  plots <- quietly(plots_append(plots_init(), dots, name = "Cars/dots", show = FALSE))
  expect_equal(nrow(plots_changes(plots)), 0)
  expect_named(plots_changes(plots), c("name", "key", "value", "default"))

  default_4 <- plots_params(plots, "Cars/dots")$colors$default[["4"]]
  plots <- plots_set(plots, "Cars/dots", title = NA, width = 9,
                     colors = c("4" = "#36c8ef"), show = FALSE)
  # Set to what it already is, so it is no change at all: a grid that shows
  # anyway, a subtitle taken off a plot that has none, the declared width.
  plots <- plots_set(plots, "Cars/dots", x_grid = TRUE, subtitle = NA, show = FALSE)
  declared <- quietly(plots_append(plots_init(), dots, name = "d", show = FALSE))
  declared <- plots_set(declared, "d", width = 15L, show = FALSE)
  expect_equal(nrow(plots_changes(declared)), 0)

  changes <- plots_changes(plots)
  expect_equal(changes$key, c("title", "width", "colors"))
  expect_equal(changes$value[[1]], NA)
  expect_equal(changes$default[[1]], "Dots")
  expect_equal(changes$value[[2]], 9)
  # Only the key that changed, not the whole mapping.
  expect_equal(changes$value[[3]], list("4" = "#36c8ef"))
  expect_equal(changes$default[[3]][["4"]], default_4)
})

test_that("the code from plots_changes_code() makes the same changes again", {
  dots <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  build <- function() {
    quietly({
      plots <- plots_init(name = "Rebuilt")
      plots <- plots_append(plots, titled, name = "Module 1/first", type = "bar", show = FALSE)
      plots_append(plots, dots, name = "Module 1/dots", show = FALSE)
    })
  }

  edited <- build() |>
    plots_set_path("results/2026-09/plots") |>
    plots_set("Module 1/first", title = "A \"quoted\" title\nover two lines",
              caption = NA, height = 7.5, show = FALSE) |>
    plots_set("Module 1/dots", colors = c("4" = "#36c8ef", "8" = "#e4572e"), show = FALSE)

  code <- plots_changes_code(edited)
  expect_s3_class(code, "plots_code")
  expect_match(code, "^plots <- plots \\|>\n  plots_reset\\(show = FALSE\\) \\|>")
  expect_match(code, 'plots_set_path("results/2026-09/plots")', fixed = TRUE)

  # Run on a fresh build, the code gives the same changes and the same path.
  plots <- build()
  quietly(eval(parse(text = code)))
  expect_equal(plots_changes(plots), plots_changes(edited))
  expect_equal(plots_meta(plots)$path, "results/2026-09/plots")
  expect_equal(plots_pull(plots, 1)$labels$title, "A \"quoted\" title\nover two lines")

  expect_equal(unclass(plots_changes_code(build())),
               "plots <- plots |>\n  plots_reset(show = FALSE)")
  expect_match(plots_changes_code(plots_init()), "no plots")

  # A change the script makes that the stored collection took away is gone again too.
  scripted <- function() plots_set(build(), "Module 1/first", subtitle = "From the script",
                                   width = 10, show = FALSE)
  stored <- quietly(plots_reset(scripted(), "Module 1/first", params = "subtitle", show = FALSE))
  stored <- plots_set(stored, "Module 1/first", width = 12, show = FALSE)
  plots <- scripted()
  quietly(eval(parse(text = plots_changes_code(stored))))
  expect_equal(plots_changes(plots), plots_changes(stored))
  expect_equal(plots_pull(plots, 1)$labels$subtitle, "Subtitle")
  # A whole number that arrived as an integer is written as a number.
  from_json <- plots_set(build(), "Module 1/first", width = 10L, show = FALSE)
  expect_match(plots_changes_code(from_json), "width = 10,", fixed = TRUE)
  expect_match(plots_changes_code(edited, object = "deck"), "^deck <- deck")
  expect_error(plots_changes_code(edited, object = "not a name"), "variable")
  expect_output(print(code), "plots_set_path")
})

test_that("plots_changes_code() writes any key as a name R reads back", {
  keys <- c("a`b", "a\\b", "caf\u00e9 2", "2nd")
  keyed <- customizer(
    apply = function(plot, ...) plot,
    params = function(plot) {
      stats::setNames(lapply(keys, function(k) param_text(k)), keys)
    }
  )
  plots <- quietly(plots_append(using(keyed), p, name = "k", show = FALSE))
  values <- stats::setNames(as.list(c("one", "two", "three", "four")), keys)
  plots <- rlang::exec(plots_set, plots, "k", !!!values, show = FALSE)
  rebuilt <- quietly(plots_append(using(keyed), p, name = "k", show = FALSE))
  quietly(eval(parse(text = plots_changes_code(plots, object = "rebuilt"))))
  expect_equal(plots_changes(rebuilt), plots_changes(plots))
  expect_setequal(plots_changes(rebuilt)$key, keys)
})

test_that("a plot whose aes() calls an unqualified package function is added with a warning", {
  skip_if_not_installed("dplyr")
  withr::local_package("dplyr")
  bare <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, desc(mpg))) +
    ggplot2::geom_point(ggplot2::aes(color = if_else(am == 1, "manual", "automatic")))
  expect_warning(
    quietly(plots_append(plots_init(), bare, name = "a", show = FALSE)),
    "plot y: desc \\(dplyr\\)"
  )
  expect_warning(
    quietly(plots_append(plots_init(), bare, name = "a", show = FALSE)),
    "layer 1 colour: if_else \\(dplyr\\)"
  )
  # Qualified calls, base R, ggplot2 and bare columns all travel.
  fine <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, dplyr::desc(mpg), fill = factor(cyl))) +
    ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)), stat = "count")
  expect_no_warning(quietly(plots_append(plots_init(), fine, name = "a", show = FALSE)))
})

test_that("a text the theme hides offers no size, and resizing leaves it hidden", {
  hidden <- titled + ggplot2::labs(y = "Mileage") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(), plot.caption = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
  plots <- quietly(plots_append(plots_init(), hidden, name = "h", show = FALSE))
  keys <- names(plots_params(plots, "h"))
  expect_false(any(c("y_size", "caption_size") %in% keys))
  expect_true(all(c("x_size", "title_size", "axis_text_size", "text_size") %in% keys))

  # The overall size and the axis text size work, and the hidden texts stay hidden.
  plots <- plots_set(plots, "h", text_size = 16, axis_text_size = 12, title_size = 20, show = FALSE)
  got <- plots_pull(plots, 1)
  expect_s3_class(got, "ggplot")
  theme <- ggplot2::complete_theme(got$theme)
  expect_s3_class(ggplot2::calc_element("axis.title.y", theme), "element_blank")
  expect_s3_class(ggplot2::calc_element("axis.text.x", theme), "element_blank")
  expect_equal(ggplot2::calc_element("axis.text.y", theme)$size, 12)
  expect_no_error(ggplot2::ggplot_build(got))
  expect_error(plots_set(plots, "h", caption_size = 9, show = FALSE), "no parameter")
})

test_that("forcats travels: a reader attaches it, so fct_rev() in aes() is fine", {
  skip_if_not_installed("forcats")
  withr::local_package("forcats")
  reversed <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, fct_rev(factor(cyl)))) + ggplot2::geom_point()
  expect_no_warning(quietly(plots_append(plots_init(), reversed, name = "a", show = FALSE)))
})

test_that("legend entries and axis categories take new names, and keep the rest", {
  bars <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(gear))) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(values = c("3" = "red", "4" = "blue", "5" = "green"))
  plots <- quietly(plots_append(plots_init(), bars, name = "b", show = FALSE))
  params <- plots_params(plots, "b")
  expect_equal(params$legend_labels$keys, c("3", "4", "5"))
  expect_equal(params$legend_labels$to, "text")
  expect_equal(params$x_labels$default, list("4" = "4", "6" = "6", "8" = "8"))
  expect_equal(params$x_labels$label, "X axis labels")
  expect_null(params$y_labels)   # a count axis is continuous

  plots <- plots_set(plots, "b", legend_labels = c("4" = "Four gears"),
                     x_labels = c("8" = "Eight cylinders"), colors = c("3" = "#36c8ef"),
                     axis_text_wrap = 6, show = FALSE)
  built <- ggplot2::ggplot_build(plots_pull(plots, 1))
  fill <- built$plot$scales$get_scales("fill")
  expect_equal(as.character(fill$get_labels()), c("3", "Four gears", "5"))
  expect_equal(toupper(unname(fill$map(c("3", "4")))), c("#36C8EF", "#0000FF"))
  # The new axis name is wrapped like the others.
  expect_equal(as.character(built$layout$panel_scales_x[[1]]$get_labels()),
               c("4", "6", "Eight\ncylinders"))

  # Flipped, the category axis is drawn as the y axis and named so.
  flipped <- quietly(plots_append(plots_init(), bars + ggplot2::coord_flip(), name = "f",
                                  show = FALSE))
  expect_equal(plots_params(flipped, "f")$x_labels$label, "Y axis labels")

  # A plot with no legend and no discrete axis offers neither.
  dots <- quietly(plots_append(plots_init(), p, name = "d", show = FALSE))
  expect_false(any(c("legend_labels", "x_labels", "y_labels") %in% names(plots_params(dots, "d"))))

  # A plot without a fill scale of its own gets the default one, with the new names.
  plain <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(am))) + ggplot2::geom_bar()
  plain <- quietly(plots_append(plots_init(), plain, name = "p", show = FALSE))
  plain <- plots_set(plain, "p", legend_labels = c("0" = "Automatic", "1" = "Manual"), show = FALSE)
  plain_fill <- ggplot2::ggplot_build(plots_pull(plain, 1))$plot$scales$get_scales("fill")
  expect_equal(as.character(plain_fill$get_labels()), c("Automatic", "Manual"))
})

test_that("an empty category is offered for neither a color nor a name", {
  one_bar <- ggplot2::ggplot(mtcars, ggplot2::aes("", fill = factor(gear))) + ggplot2::geom_bar()
  plots <- quietly(plots_append(plots_init(), one_bar, name = "o", show = FALSE))
  params <- plots_params(plots, "o")
  expect_null(params$x_labels)
  expect_equal(params$legend_labels$keys, c("3", "4", "5"))

  blank_level <- data.frame(g = c("", "a", "b"), n = 1:3)
  filled <- ggplot2::ggplot(blank_level, ggplot2::aes(g, n, fill = g)) + ggplot2::geom_col()
  plots <- quietly(plots_append(plots_init(), filled, name = "f", show = FALSE))
  params <- plots_params(plots, "f")
  expect_equal(params$colors$keys, c("a", "b"))
  expect_equal(params$x_labels$keys, c("a", "b"))
  expect_s3_class(plots_pull(plots_set(plots, "f", colors = c(a = "#36c8ef"), show = FALSE), 1),
                  "ggplot")
})

test_that("the numbers on a plot take a size, and grow with the overall text size", {
  counted <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar() +
    ggplot2::geom_text(ggplot2::aes(label = ggplot2::after_stat(count)), stat = "count", size = 4) +
    ggplot2::geom_label(ggplot2::aes(label = ggplot2::after_stat(count)), stat = "count",
                        size = 10, size.unit = "pt")
  plots <- quietly(plots_append(plots_init(), counted, name = "c", show = FALSE))
  param <- plots_params(plots, "c")$data_label_size
  expect_equal(param$default, round(4 * 72.27 / 25.4 * 2) / 2)
  sizes <- function(plots) {
    built <- ggplot2::ggplot_build(plots_pull(plots, 1))
    c(unique(built$data[[2]]$size), unique(built$data[[3]]$size))
  }

  # Points, whatever unit each layer draws in.
  set <- plots_set(plots, "c", data_label_size = 16, show = FALSE)
  expect_equal(sizes(set), c(16 / (72.27 / 25.4), 16))

  # The overall size scales them by its own ratio, and a specific size still wins.
  base_text <- ggplot2::calc_element("text", ggplot2::complete_theme(plots_pull(plots, 1)$theme))$size
  doubled <- plots_set(plots, "c", text_size = base_text * 2, show = FALSE)
  expect_equal(sizes(doubled), c(8, 20))
  both <- plots_set(plots, "c", text_size = base_text * 2, data_label_size = 12, show = FALSE)
  expect_equal(sizes(both), c(12 / (72.27 / 25.4), 12))

  # The stored plot keeps its own sizes.
  expect_equal(sizes(plots), c(4, 10))

  # A layer that maps size to the data is left alone, and a plot without text offers nothing.
  mapped <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, label = cyl, size = cyl)) +
    ggplot2::geom_text()
  mapped <- quietly(plots_append(plots_init(), mapped, name = "m", show = FALSE))
  expect_null(plots_params(mapped, "m")$data_label_size)
  expect_null(plots_params(quietly(plots_append(plots_init(), p, name = "p", show = FALSE)),
                           "p")$data_label_size)
})

test_that("the canvas goes up to 30 inches a side", {
  plots <- quietly(plots_append(plots_init(), p, name = "a", show = FALSE))
  expect_equal(plots_params(plots, "a")$width$max, 30)
  expect_no_error(plots_set(plots, "a", width = 30, height = 30, show = FALSE))
  expect_error(plots_set(plots, "a", width = 31, show = FALSE), "30")
})
