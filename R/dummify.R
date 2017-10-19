#' Generate a data.frame of dummy variables given a vector
#'
#' Generate a data.frame of dummy variables given a vector
#'
#' @param x a vector
#' @param prefix name prefix of returned \code{\link{data.frame}} columns
#' @return a \code{\link{data.frame}} to be used with \code{\link{cbind}}
#' @examples
#'
#' rep(1:3, 4)
#' dummify(rep(1:3, 4))
#'
#' @export
dummify <- function(x, prefix = ""){
    if ((xclass <- class(x)) %nin% c("integer", "factor", "ordered"))
        stop("only integer, factor or ordered vectors")
    
    if (! (is.character(prefix) && (length(prefix) == 1)))
        stop("prefix must be a 1-length character vector")
    
    if ("integer" == xclass)
        x <- factor(x)
    
    xlevels <- gsub(" ", "_", levels(x))
    res <- as.data.frame(model.matrix(~ x - 1))
    names(res) <- paste0(prefix, xlevels)
    res
}

