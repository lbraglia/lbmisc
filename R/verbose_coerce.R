#' Coerce/convert data verbosely if NA is introduced by coercion
#' 
#' Coerce/convert a given x using the coercer function; if a
#' warning is raised and a difference in NA cardinality is 
#' present a report of NA produced by coercion is printed
#' 
#' @param x data to
#' @param coercer a function (tipically as.*)
#' @examples \dontrun{verbose_coerce(c("a", 2L, 3L), as.integer)}
#' 
#' @export
verbose_coerce <- function(x, coercer){
    xc <- tryCatch(coercer(x),
                   ## verbose message handling
                   warning = function(warn) {
                       warning(warn)
                       xcoerced <- suppressWarnings(coercer(x))
                       rows <- !is.na(x) & is.na(xcoerced)
                       if (any(rows)){
                           cat('Unique differences: \n\n')
                           df <- data.frame('original' = x, 
                                            'coerced'  = xcoerced)
                           print(unique(df[rows, ]), row.names = FALSE)
                           cat("\n")
                       }
                       ## return value
                       return(xcoerced)
                   })
    xc
}
