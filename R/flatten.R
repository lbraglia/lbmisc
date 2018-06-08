#' flatten a list of nested list
#' 
#' flatten a list of nested list
#'
#' @param x a list of nested list
#' @source https://stackoverflow.com/questions/16300344
#' @export
flatten <- function(x) do.call(c, unlist(x, recursive = FALSE))
