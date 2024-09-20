#' Create a lens
#'
#' A lens is a pair of functions that can be used to view and set a value in an object.
#'
#' @param view A function that takes an object and returns a value
#' @param set A function that takes an object and a value and returns a new object
#' @importFrom S7 new_class class_function
#' @export
lens <- S7::new_class("lens",
                      package = packageName(),
                      properties = list(
                        "view" = class_function,
                        "set" = class_function
                      ))
