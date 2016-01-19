#' remove NA and make a message with number of units deleted
#'
#' remove NA and make a message with number of units deleted
#' @param x something handled by \code{stats::na.omit}
#' @param verbose logical: if \code{TRUE} raise a message if any row is
#'     deleted
#'@export
remove_NA <- function(x, verbose = TRUE){
    y  <- stats::na.omit(x)
    nx <- nrow(x)
    ny <- nrow(y)
    if (verbose)
        if (nx != ny)
            message('Rows were' , nx, ', now are ', ny, '. ',
                    nx - ny, ' rows deleted due missingness.')
    return(y)
}
