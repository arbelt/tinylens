#' include verbs.R
#' include lens.R

#' Select lens
#'
#' This function returns a lens that selects the specified columns.
#'
#' @param ... Columns to select
#'
#' @return A lens that selects the specified columns
#'
#' @importFrom rlang expr
#' @export
select_l <- function(...) {
  .dots <- rlang::enexprs(...)
  .get_cols <- function(x) {
    .cols <- tidyselect::eval_select(expr = expr(c(!!!.dots)), data = x)
  }
  getter <- function(x) {
    .cols <- .get_cols(x)
    x[, .cols, drop = FALSE]
  }
  setter <- function(x, value) {
    if (nrow(x) < 1L) return(x)
    .cols <- .get_cols(x)
    x[, .cols] <- value
    x
  }
  lens(view = getter, set = setter)
}

#' Rows lens
#'
#' This function returns a lens that selects the specified rows.
#'
#' @param idx The rows to select
#'
#' @return A lens that selects the specified rows
#'
#' @export
rows_l <- function(idx) {
  getter <- function(d) {
    d[idx, , drop = FALSE]
  }
  setter <- function(d, value) {
    if (nrow(d) < 1L) return(d)
    d[idx, ] <- value
    d
  }
  lens(view = getter, set = setter)
}

#' Filter ilens
#'
#' This function returns an illegal lens that filters according to the specified conditions.
#' 
#' Conditions are evaluated in the context of the data frame.
#' 
#' @param ... Conditions to filter by
#'
#' @return A lens that filters the specified rows
#'
#' @export
filter_il <- function(...) {
  .dots <- rlang::enexprs(...)
  .get_rows <- function(d) {
    .evaled_dots <- Map(function(x) {
      rlang::eval_tidy(x, data = d)
    }, .dots)
    .rows <- Reduce(`&`, .evaled_dots)
    .rows
  }
  getter <- function(d) {
    .rows <- .get_rows(d)
    d[.rows, , drop = FALSE]
  }
  setter <- function(d, value) {
    if (nrow(d) < 1L) return(d)
    .rows <- .get_rows(d)
    d[.rows, ] <- value
    d
  }
  lens(view = getter, set = setter)
}
