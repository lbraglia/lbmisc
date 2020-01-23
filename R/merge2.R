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
#' @param id_dupl character, warning if duplicated id are found for
#'     the selected data.frame
#' @param id_not_found message if some A id are not found in B (eg
#'     x_nin_y or y_nin_x)
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
                              id_dupl = c('x','y'),
                              id_not_found = c('y_nin_x'),
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
    
    if ('x' %in% id_dupl){
        dupl_x <- x_ids[duplicated(x_ids), , drop = FALSE]
        rownames(dupl_x) <- NULL
        dupl_ids(dupl_x, 'x')
    }

    if ('y' %in% id_dupl){
        dupl_y <- y_ids[duplicated(y_ids), , drop = FALSE]
        rownames(dupl_y) <- NULL
        dupl_ids(dupl_y, 'y')
    }

    ## ----------------------------------------------
    ## message for IDS not found in the other dataset
    ## ----------------------------------------------

    if ('x_nin_y' %in% id_not_found) a_rows_not_in_b(x_ids, y_ids, 'x', 'y')
    if ('y_nin_x' %in% id_not_found) a_rows_not_in_b(y_ids, x_ids, 'y', 'x')

    ## -----------------------------------------------------------
    ## message for same names (other than IDS) in the two datasets
    ## -----------------------------------------------------------
    x_not_ids <- names(x) %without% by.x
    y_not_ids <- names(y) %without% by.y
    message_dupl_varnames(x_not_ids, y_not_ids)
    
    ## ----------------------------------
    ## otherwise it's a standard merge...
    ## ----------------------------------
    
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

## helper functions
dupl_ids <- function(z, who){
    if (nrow(z) > 0L){
        cat("\n=============================================\n")
        warning('Duplicates ids in ', who)
        cat("=============================================\n")
        print(unique(z))
        cat("=========================================\n")
    } else invisible(NULL)
}

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

message_dupl_varnames <- function(nx, ny) {
    dupl <- intersect(nx, ny)
    if (length(dupl) > 0L) {
        cat("\n=============================================\n")
        message('Variable (other than IDS) with the same name')
        cat("=============================================\n")
        cat(paste(dupl, collapse = ', '))
        cat("\n=============================================\n")
    }
}
