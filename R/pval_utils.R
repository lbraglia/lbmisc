#' Pretty print for p-values
#' 
#' Pretty print for p-values.
#' 
#' @param pvalue A numeric vector of p-values.
#' @param digits number of digits returned (3 by default).
#' @param space Logical specifying whether a space should be inserted between
#' number and operator (= or <, default to FALSE).
#' @param equal add = where needed?
#' @return The function returns a string with the pretty printed p-values.
#' @examples
#'
#' pval1 <- c(3, NA, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
#' pretty_pval(pval1, space = TRUE)
#' 
#' @export
pretty_pval <- function(pvalue, digits = 3L, space = FALSE, equal = FALSE) {

    old.scipen <- options('scipen')
    on.exit(options('scipen' = old.scipen))
    options('scipen' = 999) 
    
    ## Pretty printing for p-value 
    if (any(wrong <- (pvalue> 1 | pvalue <0), na.rm = TRUE)) {
        warning('Not all p-values in [0,1], ')
        pvalue[wrong] <- NA
    }
    ## Digits should be at least 1L
    digits <- as.integer(digits)
    if (digits < 1L)
        stop('Digits should be at least 1L')

    worker <- function(x, space, equal) {
        if (is.na(x)) {
            return(NA_character_)
        } else if (x < 10L^(-digits)) {
            return(paste0('<', if (space) ' ' else '',
                          as.character(10L^(-digits))))
        } else if (x < 1) {
            fmt <- sprintf('%%.%df', digits)
            char <- sprintf(fmt, x)
            return(paste0(
                if (equal) '=' else '',
                if (space) ' ' else '',
                char))
        } else {
            return(paste0(
                if (equal) '=' else '',
                if (space) ' ' else '',
                '1'))
        }
    }
    
    unlist(lapply(pvalue, worker, space = space, equal = equal))
}


#' Pretty print for p-values
#' 
#' Pretty print for p-values.
#' 
#' @param p A numeric vector of p-values.
#' @return The function returns a character vector with stars related
#'         to p-value.
#' @examples
#'
#' pval1 <- c(3, NA, 0.005, 0.025, 0.05, 0.07, 0.5)
#' pval_stars(pval1)
#' 
#' @export
pval_stars <- function(p = NULL){
    res <- cut(p, 
               breaks = c(0, 0.01, 0.05, 0.1, 1),
               labels = c('***', '**', '*', ''))
    as.character(res)
}
