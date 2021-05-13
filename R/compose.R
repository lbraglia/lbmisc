#' compose functions passed to ...
#' 
#' @param ... functions to be composed
#' @examples
#' add2 <- function(x) x+2
#' mul2 <- function(x) x*2
#' f = compose(add2, mul2)
#' g = compose(mul2, add2)
#' f(1:3)
#' g(1:3)
#' @export
compose <- function(...){
    ## R version of Python Functional Programming HOWTO
    funs <- list(...)
    worker <- function(x){
        result <- x
        revfuns <- funs[rev(seq_along(funs))]
        for (f in revfuns){
            result = f(result)
        }
        result
    }
    worker
}

