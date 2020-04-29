#' Coerce/convert data verbosely if NA is introduced by coercion
#' 
#' Coerce/convert a given x using the coercer function; if a warning
#' is raised and a difference in NA cardinality is present a report of
#' NA produced by coercion is printed, with optional rows id printed
#' 
#' @param x vector to be converted
#' @param coercer a function (tipically as.*)
#' @param xname string used for progress (varname) printing
#' @param varname_msg logical: print progress (varname)?
#' @param row_id vector or data.frame used to identify the coerced
#'     rows (for reporting purposes in case of issues)
#'
#' @examples \dontrun{
#' verbose_coerce(c("a", 2L, 3L), as.integer)
#' verbose_coerce(c("a", 2L, 3L), as.integer, row_id = letters[1:3])
#' }
#'
#' @export
verbose_coerce <- function(x, coercer,
                           xname = NULL,
                           varname_msg = TRUE,
                           row_id = NULL
                           ){
    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))
    if (varname_msg) cat(sprintf("Processing %s\n", xname))

    xc <- coercer(x)
    problem <- (!is.na(x)) & is.na(xc)
    if (any(problem)){
        if (!is.null(row_id)){
            if (is.data.frame(row_id))
                df <- cbind(row_id, data.frame('original' = x,
                                               'coerced'  = xc))
            else
                df <- data.frame('id' = row_id,
                                 'original' = x,
                                 'coerced'  = xc)
        } else 
            df <- data.frame('original' = x,
                             'coerced'  = xc)
        cat('Unique differences: \n\n')
        print(unique(df[problem, , drop = FALSE]), row.names = FALSE)
        cat("\n")
    }
       
    xc
}
