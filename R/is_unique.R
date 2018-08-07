#' Utility function to check whether an object is unique
#'
#' @param x the object
#' 
#' @export
is.unique <- function(x = NULL) {
	!(any(duplicated(x)))
}
