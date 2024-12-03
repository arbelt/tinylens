#' @include verbs.R
#' @include lens.R
NULL


#' Identity lens
#'
#' Trivial identity lens: returns and sets the object itself.
#'
#' @export
#' @examples
#' x <- 1:10
#' view(x, id_l)
id_l <- lens(
  view = identity,
  set = function(., x) x
)

#' Names lens
#'
#' Lens into the `names` attribute of an object. This uses `rlang::names2` to
#' better handle `NULL` names.
#'
#' @importFrom rlang names2
#' @export
#' @examples
#' x <- letters[1:10]
#' names(x) <- letters[1:10]
#' view(x, names_l)
#' over(x, names_l, toupper)
names_l <- lens(
  view = names2,
  set = rlang::`names2<-`
)

#' Attributes lens
#'
#' Lens into a named attribute of an object.
#'
#' @param name Name of the attribute to lens into
#'
#' @return A lens that selects the specified attribute
#'
#' @export
#' @examples
#' x <- 1:10
#' attr(x, "label") <- "my_label"
#' l <- attr_l("label")
#' view(x, l)
#' set(x, l, "new_label")
attr_l <- function(name) {
  lens(
    view = function(x) attr(x, name, exact = TRUE),
    set = function(x, value) {
      attr(x, name) <- value
      x
    }
  )
}

#' Slice lens
#'
#' Lens into a slice of a vector.
#'
#' This lens performs indexing using single bracket notation, i.e., `x[idx]`.
#'
#' @param idx Indices of the elements to lens into
#' @return A lens that selects the specified slice
#'
#' @export
#' @examples
#' x <- letters[1:10]
#' l <- slice_l(1:5)
#' view(x, l)
slice_l <- function(idx) {
  lens(
    view = function(d) vctrs::vec_slice(d, idx),
    set = function(d, value) {
      vctrs::vec_assign(d, idx, value)
    }
  )
}

#' Index lens
#'
#' Lens into a single element of a list.
#'
#' This lens performs indexing using double bracket notation, i.e., `x[[i]]`.
#'
#' @param i Index of the element to lens into
#'
#' @return A lens that selects the specified element
#'
#' @export
#' @examples
#' x <- list(a = 1, b = 2)
#' l <- index_l("a")
#' view(x, l)
index_l <- function(i) {
  lens(
    view = function(x) x[[i]],
    set = function(x, value) {
      x[[i]] <- value
      x
    }
  )
}

#' Vector data lens
#'
#' Allows mutation of vector data while preserving attributes, e.g., labels or names.
#'
#' @examples
#' x <- letters[1:10]
#' names(x) <- letters[1:10]
#' # toy function that strips names; most functions from `stringr` do this
#' f <- function(x) toupper(unname(x))
#' # apply the function without losing attributes
#' over(x, vec_data_l, f)
#' @export
vec_data_l <- lens(
  view = vctrs::vec_data,
  set = function(d, value) {
    .attrs <- attributes(d)
    new_d <- vctrs::vec_data(d)
    new_d[] <- value
    attributes(new_d) <- .attrs
    new_d
  }
)

#' Lens into a list or vector
#'
#' This lens allows you to access and modify elements of a list or vector
#' based on their position or a logical condition.
#'
#' @param l A lens that selects the elements to lens into
#' @param .ptype The prototype of the data structure to return
#' @return A lens that selects the specified elements
#'
#' @export
#' @examples
#' d <- list(list(a = 1, b = 2), list(a = 4, b = 9))
#' l <- index_l("a")
#' view(d, map_l(l))
#' over_map(d, map_l(l), sqrt)
map_l <- function(l, .ptype = NULL) {
  .view <- function(d) {
    new_d <- d
    new_d[] <- lapply(d, \(x) view(x, l))
    if (!is.list(d) && all(vapply(new_d, rlang::is_scalar_atomic, logical(1)))) {
      return(unlist(new_d, recursive = FALSE))
    }
    if (rlang::is_atomic(.ptype) && all(vapply(new_d, rlang::is_scalar_atomic, logical(1)))) {
      return(unlist(new_d, recursive = FALSE))
    }
    new_d
  }
  .setter <- function(d, x) {
    if (is.null(x)) {
      x <- list(NULL)
    }
    new_d <- d
    new_d[] <- mapply(l@set, d, x, SIMPLIFY = FALSE)
    if (!is.list(d) && all(vapply(new_d, rlang::is_scalar_atomic))) {
      return(unlist(new_d, recursive = FALSE))
    }
    if (rlang::is_atomic(.ptype) && all(vapply(new_d, rlang::is_scalar_atomic))) {
      return(unlist(new_d, recursive = FALSE))
    }
    new_d
  }
  lens(
    view = .view,
    set = .setter
  )
}


#' Lens for accessing and modifying nested elements of a list or vector
#'
#' Convenience function that mirrors [purrr::pluck()].
#'
#' @param ... A sequence of lenses and/or integers/logical vectors
#' @return A lens that combines all specified lenses (left to right).
#'
#' @export
#' @examples
#' d <- list(a = list(b = 1, c = 2), b = list(b = 3, c = 4))
#' l <- c_l("a", "b")
#' view(d, l)
c_l <- function(...) {
  dots <- list(...)
  Reduce(function(acc, x) {
    if (inherits(x, "lens")) {
      return(acc %.% x)
    }

    if (!is.vector(x) || is.null(x)) {
      stop("`c_l` expects all arguments to be either a lens or atomic vector")
    }

    if (length(x) == 1 && !is.logical(x) && !(is.numeric(x) && x < 0)) {
      return(acc %.% index_l(x))
    }

    return(acc %.% slice_l(x))
  }, dots, id_l)
}

#' Predicate ilens
#'
#' Illegal lens into elements of a vector that satisfy a predicate.
#'
#' @param p A predicate function
#'
#' @return A lens that selects the elements that satisfy the predicate
#'
#' @export
#' @examples 
#' d <- 1:10
#' l <- where_il(\(x) x %% 2 == 0)
#' view(d, l)
#' over(d, l, \(x) x / 2)
where_il <- function(p) {
  get_idx <- function(d) {
    result <- unlist(lapply(d, p))
    stopifnot(is.logical(result))
    result
  }
  getter <- function(d) {
    idx <- get_idx(d)
    d[idx]
  }
  setter <- function(d, x) {
    idx <- get_idx(d)
    d[idx] <- x
    d
  }
  lens(view = getter, set = setter)
}
