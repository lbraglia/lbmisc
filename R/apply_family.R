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
        cat("\n\nProcessing", n, "\n\n")
        FUN(v, ...)
    }, x, x_names)
    rval 
}


#' apply a function by group and return results of the same length to
#' the data
#' @param x same as tapply
#' @param group a factor
#' @param f function to be applied
#' @param ... optional arguments passed to the function
#' 
#' @examples
#' set.seed(123)
#' data <- data.frame(y = 1:10, g = gl(2,5, labels = c("1-5", "6-10")))
#' data <- data[sample(nrow(data)), ]
#' data$mean  <- with(data, group_f(y, g, mean))
#' data$min  <- with(data, group_f(y, g, min))
#' data$max  <- with(data, group_f(y, g, max))
#'
#' @export
group_f <- function(x, group, f, ...){
    if (!is.factor(group)) stop("group must be a factor")
    res <- tapply(x, group, f, ...)
    res[as.character(group)]
}

