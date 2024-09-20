#' @include verbs.R
#' @include lens.R


#' Identity lens
#'
#' @export
id_l <- lens(
  view = identity,
  set = function(., x) x
)

#' Names lens
#'
#' Lens into the `names` attribute of an object.
#'
#' @importFrom rlang names2
#' @export
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
index_l <- function(i) {
  lens(
    view = function(x) x[[i]],
    set = function(x, value) {
      x[[i]] <- value
      x
    }
  )
}

#' Lens into vector data
#'
#' Allows mutation of vector data while preserving attributes, e.g., labels.
#'
#' @export
vec_data_l <- lens(
  view = vctrs::vec_data,
  set = function(d, value) {
    .attrs <- attributes(d)
    attributes(value) <- .attrs
    value
  }
)

#' Lens into a list or vector
#'
#' This lens allows you to access and modify elements of a list or vector
#' based on their position or a logical condition.
#'
#' @param l A lens that selects the elements to lens into
#'
#' @return A lens that selects the specified elements
#'
#' @export
map_l <- function(l) {
  .view <- function(d) {
    new_d <- lapply(d, \(x) view(x, l))
    if (!is.list(d) && all(vapply(new_d, rlang::is_scalar_atomic))) {
      return(unlist(new_d, recursive = FALSE))
    }
    new_d
  }
  .setter <- function(d, x) {
    if (is.null(x)) {
      x <- list(NULL)
    }
    new_d <- mapply(l@set, d, x, SIMPLIFY = FALSE)
    if (!is.list(d) && all(vapply(new_d, rlang::is_scalar_atomic))) {
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
#'
#' @export
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
