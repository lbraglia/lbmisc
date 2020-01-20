#' A verbose merge
#' 
#' A verbose merge
#' 
#' @param x first element
#' @param y second element
#' @param ... further arguments passed to methods
#' 
#' @export merge2
merge2 <- function(x, y, ...) {
    UseMethod("merge2")
}

#' @export merge2.default
merge2.default <- function(x, y, ...) 
    merge2(as.data.frame(x), as.data.frame(y), ...)

#' @param x same as in \code{merge}
#' @param y same as in merge
#' @param by same as in merge
#' @param by.x same as in merge
#' @param by.y same as in merge
#' @param all same as in merge
#' @param all.x same as in merge
#' @param all.y same as in merge
#' @param sort same as in merge
#' @param suffixes same as in merge
#' @param no.dups same as in merge
#' @param incomparables same as in merge
#' @param ... same as in merge
#' @export merge2.data.frame
merge2.data.frame <- function(x, y,
                              by = intersect(names(x), names(y)),
                              by.x = by,
                              by.y = by,
                              all = FALSE,
                              all.x = all,
                              all.y = all,
                              sort = TRUE,
                              suffixes = c(".x",".y"),
                              no.dups = TRUE,
                              incomparables = NULL, ...)
{

    ## ----------------------
    ## handle duplicated IDS
    ## ----------------------
    dupl_x <- x[duplicated(x[, by.x]), by.x, drop = FALSE]
    dupl_y <- y[duplicated(y[, by.y]), by.y, drop = FALSE]

    dupl_ids <- function(z, who){
        if (nrow(z) > 0L){
            warning('Duplicates ID in ', who, '; unique IDs are\n')
            print(unique(z))
            cat("\n")
        } else invisible(NULL)
    }

    dupl_ids(dupl_x, 'x')
    dupl_ids(dupl_y, 'y')

    ## ----------------------
    ## standard merge
    ## ----------------------
    
    merge.data.frame(x = x,
                     y = y,
                     by = by,
                     by.x = by.x,
                     by.y = by.y,
                     all = all,
                     all.x = all.x,
                     all.y = all.y,
                     sort = sort,
                     suffixes = suffixes,
                     no.dups = no.dups,
                     incomparables = incomparables,
                     ...)
                     
}
    

