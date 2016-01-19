#' Inverse Value Matching
#'
#' 
#' Complement of \code{\%in\%}. Returns the elements of \code{x}
#' that are not in \code{y}.
#'
#' 
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname nin
"%nin%" <- function(x, y) {
    return( !(x %in% y) )
}

#' Check if in range
#'
#' Check if in range
#' 
#' @param x a vector
#' @param y a vector of 2 values
#' @examples
#' x <- 1:10
#' y <- c(4L, 7L)
#' x %in_range% y
#' @rdname in_range
#' @export
"%in_range%" <- function(x, y) {
  stopifnot(length(y) == 2L)
  return( (x >= y[1]) & (x <= y[2]))
}

#' Remove elements from a vector, chosing from another vector
#' 
#' Return a vector x without the commont elements to another vector y.
#'
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname without
"%without%" <-  function(x, y) x[!(x %in% y)]


