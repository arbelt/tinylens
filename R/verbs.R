#' @include lens.R
#' @importFrom S7 new_generic method method<-
#' @importFrom S7 S7_dispatch
NULL

#' View data through a lens
#'
#' This function applies a lens to a data structure and returns the focused part.
#'
#' @param d The data structure to view
#' @param l The lens to apply
#'
#' @return The part of the data structure focused by the lens
#'
#' @export
view <- new_generic("view", c("d", "l"), function(d, l) {
  S7_dispatch()
})

#' Set data through a lens
#'
#' This function applies a lens to a data structure and sets the focused part.
#'
#' @param d The data structure to set
#' @param l The lens to apply
#' @param x The value to set
#'
#' @return The modified data structure
#' @export
set <- new_generic("set", c("d", "l", "x"), function(d, l, x) {
  S7_dispatch()
})

#' Modify data through a lens
#'
#' This function applies a lens to a data structure and modifies the focused part.
#'
#' @param d The data structure to modify
#' @param l The lens to apply
#' @param f The function to apply
#'
#' @return The modified data structure
#' @export
over <- new_generic("over", c("d", "l", "f"), function(d, l, f) {
  S7_dispatch()
})

#' @importFrom S7 class_any method method<-
method(view, list(class_any, lens)) <- function(d, l) {
  l@view(d)
}

method(set, list(class_any, lens, class_any)) <- function(d, l, x) {
  l@set(d, x)
}

method(over, list(class_any, lens, class_function)) <- function(d, l, f) {
  l@set(d, f(l@view(d)))
}


#' Compose two lenses
#'
#' The resulting lens first applies the *left* lens, then the right lens.
#'
#' @param l First lens
#' @param m Second lens
#' @return A new lens
#' @export
`%.%` <- new_generic("%.%", c("l", "m"), function(l, m) {
  S7_dispatch()
})

#' @importFrom S7 method method<-

method(`%.%`, list(lens, lens)) <- function(l, m) {
  lens(
    view = function(data) {
      m@view(l@view(data))
    },
    set = function(data, value) {
      l@set(data, m@set(l@view(data), value))
    }
  )
}


#' Map a function over a list lens
#'
#' Apply a function to each element of a list returned by a lens. Using `over`
#' in such cases would require a "lifted" function, which is often unergonomic.
#'
#' @param d The data structure to modify
#' @param l The list-returning lens to apply
#' @param f The function to apply to each element of the list
#'
#' @return The modified data structure
#' @export
over_map <- function(d, l, f) {
  sd <- view(d, l)
  if (!is.list(sd)) {
    stop("`over_map` can only be used with a lens that returns a list.")
  }
  set(d, l, lapply(sd, f))
}
