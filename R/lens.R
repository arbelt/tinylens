#' Create a lens
#'
#' A lens is a pair of functions that can be used to view and set a value in an object.
#' Lenses are implemented as S7 classes.
#'
#' A "proper" lens should satisfy the following so-called "lens laws":
#'
#' - **View-Set**: `set(d, l, view(d, l)) == d`
#' - **Set-View**: `view(set(d, l, x), l) == x`
#' - **Set-Set**: `set(set(d, l, x), l, y) == set(d, l, y)`
#'
#' These laws are not enforced by `tinylens`, but you should strive to follow them
#' when creating your own lenses.
#'
#' A best effort has been made to ensure that these laws hold for the lenses
#' provided by `tinylens`, but this is trickier than it might seem because of
#' how R handles subset assignments.
#'
#' @param view A function that takes an object and returns a value
#' @param set A function that takes an object and a value and returns a new object
#' @importFrom S7 new_class class_function
#' @export
#' @examples
#' # create a trivial identity lens
#' l <- lens(view = function(x) x, set = function(x, value) value)
lens <- S7::new_class("lens",
  package = packageName(),
  properties = list(
    "view" = NULL | class_function,
    "set" = NULL | class_function
  ),
  constructor = function(view, set = NULL) {
    S7::new_object(
      S7::S7_object(),
      view = view,
      set = set
    )
  }
)
