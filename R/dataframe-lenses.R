#' include verbs.R
#' include lens.R

#' Select lens
#'
#' This function returns a lens that selects the specified columns. Requires
#' `tidyselect` to be installed.
#'
#' @param ... Columns to select
#'
#' @return A lens that selects the specified columns
#'
#' @importFrom rlang expr
#' @export
#' @examples
#' d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
#' l <- select_l(x, y)
#' # get the x and y columns
#' view(d, l)
#' # set the x and y columns
#' set(d, l, 1)
select_l <- function(...) {
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("tidyselect is required for select_l")
  }
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
#' @examples 
#' d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
#' l <- rows_l(1:2)
#' # get the first two rows
#' view(d, l)
#' # set the first two rows
#' set(d, l, 1:2)
rows_l <- function(idx) {
  getter <- function(d) {
    d[idx, , drop = FALSE]
  }
  setter <- function(d, value) {
    if (nrow(d) < 1L) {
      return(d)
    }
    d[idx, ] <- value
    d
  }
  lens(view = getter, set = setter)
}

#' Subset lens
#'
#' This function returns a lens that subsets the object in a generalized way.
#'
#' @param ... Conditions to subset by. Unnamed arguments are used as indices.
#'      Named arguments are passed along to `[` for viewing and are removed for
#'      setting.
#'
#' @return A lens that subsets the object by the specified indices
#'
#' @export
#' @examples
#' d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
#' l <- indices_l(1, 1)
#' # get the first row of first column
#' view(d, l)
#' # set the first row of first column
#' set(d, l, 1)
#' 
#' # get the first row
#' l <- indices_l(1,)
#' view(d, l)
#' # set the first row
#' set(d, l, 1)
indices_l <- function(...) {
  .dots <- rlang::enexprs(...)
  nArgs <- nargs()
  nEmpty <- nArgs - length(.dots)
  .dots <- append(.dots, rep(list(missing_arg()), nEmpty))
  .unnamed_dots <- .dots[names(.dots) == ""]
  getter <- function(d) {
    eval_bare(expr(d[!!!.dots]))
  }
  setter <- function(d, value) {
    eval_bare(expr(d[!!!.unnamed_dots] <- value))
    d
  }
  lens(view = getter, set = setter)
}


#' @export
#' @rdname indices_l
i_l <- indices_l

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
#' @examples
#' d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
#' l <- filter_il(x > 5)
#' # get the rows where x is greater than 5
#' view(d, l)
#' # set the rows where x is greater than 5 to 8
#' set(d, l, 8)
#' # set y value to 8 where x is greater than 5
#' set(d, l %.% select_l(y), 8)
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
