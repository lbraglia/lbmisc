#' Print names of the list elements as progress while simulating
#' lapply to a list
#'
#' @param x list
#' @param FUN function to be applied
#' @param ... further parameters passed to FUN
#' 
#' @export
verbose_lapply <- function(x, FUN, ...){
    x_names <- names(x)
    rval <- Map(function(v, n) {
        message("\n\nProcessing ", n, "\n")
        FUN(v, ...)
    }, x, x_names)
    rval 
}
