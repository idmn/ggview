test_that("save_ggplot works with normal ggplot", {
  library(ggplot2)
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("My Plot")
  temp_file <- tempfile(fileext = ".png")
  save_ggplot(p, temp_file)
  expect_true(file.exists(temp_file))
  file.remove(temp_file)
})

test_that("save_ggplot works with canvas", {
  library(ggplot2)
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("My Plot") +
    canvas(8, 6, dpi = 400)
  temp_file <- tempfile(fileext = ".png")
  save_ggplot(p, temp_file)
  expect_true(file.exists(temp_file))
  file.remove(temp_file)
})

test_that("save_ggplot saves with correct dimensions", {
  skip_if_not_installed("png")
  library(ggplot2)
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("My Plot") +
    canvas(800, 600, units = "px")
  temp_file <- tempfile(fileext = ".png")
  save_ggplot(p, temp_file)
  expect_true(file.exists(temp_file))
  info <- png::readPNG(temp_file)
  expect_equal(dim(info)[1:2], c(600, 800))
  file.remove(temp_file)
})
