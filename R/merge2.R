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

    x_ids <- x[, by.x, drop = FALSE]
    y_ids <- y[, by.y, drop = FALSE]
    
    ## --------------------------
    ## warning for duplicated IDS
    ## --------------------------
    dupl_x <- x_ids[duplicated(x_ids), , drop = FALSE]
    dupl_y <- y_ids[duplicated(y_ids), , drop = FALSE]

    rownames(dupl_x) <- NULL
    rownames(dupl_y) <- NULL
    
    dupl_ids <- function(z, who){
        if (nrow(z) > 0L){
            cat("\n=============================================\n")
            warning('Duplicates ids in ', who)
            cat("=============================================\n")
            print(unique(z))
            cat("=========================================\n")
        } else invisible(NULL)
    }

    dupl_ids(dupl_x, 'x')
    dupl_ids(dupl_y, 'y')

    ## ----------------------------------------------
    ## message for IDS not found in the other dataset
    ## ----------------------------------------------
    a_rows_not_in_b <- function(a, b, aname, bname) {
        res <- a[do.call(paste0, a) %nin% do.call(paste0, b), , drop = FALSE]
        if (nrow(res) > 0L){
            cat("\n=========================================\n")
            message(aname, "'s ids not available in ", bname)
            cat("=========================================\n")
            print(res)
            cat("=========================================\n")
        }
    }
   
    ## x_ids %nin% y_ids
    a_rows_not_in_b(x_ids, y_ids, 'x', 'y')
    a_rows_not_in_b(y_ids, x_ids, 'y', 'x')
    
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
