#' Coerce/convert data verbosely if NA is introduced by coercion
#' 
#' Coerce/convert a given x using the coercer function; if a
#' warning is raised and a difference in NA cardinality is 
#' present a report of NA produced by coercion is printed
#' 
#' @param x vector to be converted
#' @param coercer a function (tipically as.*)
#' @param xname string used for progress (varname) printing
#' @param varname_msg logical: print progress (varname)?
#'
#' @examples \dontrun{verbose_coerce(c("a", 2L, 3L), as.integer)}
#'
#' @export
verbose_coerce <- function(x, coercer,
                           xname = NULL,
                           varname_msg = TRUE
                           ){
    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))
    if (varname_msg) cat(sprintf("Processing %s\n", xname))

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
                           print(unique(df[rows, , drop = FALSE]),
                                 row.names = FALSE)
                           cat("\n")
                       }
                       ## return value
                       return(xcoerced)
                   })
    xc
}
