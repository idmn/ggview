# copied from https://github.com/tidyverse/rlang/blob/main/R/operators.R

#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @name op-null-default
#' @noRd 
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
