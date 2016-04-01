#' Smarty Multiple Response Splitter
#'
#' This function split a variable containing Smarty's output for
#' a multiple response question in a dummy data.frame, to be
#' used with \code{\link{cbind}}.
#'
#' @param x variable to be splitted
#' @param spl_char splitting character
#' @param categs character vector listing the categories to be
#'     considered and/or they order; if \code{NULL} (default) all
#'     categories given in x will be used (in alphabetic order)
#' @param add_count add a variable for
#'     considered and/or they order; if \code{NULL} (default) all
#'     categories given in x will be used (in alphabetic order)
#' @export
smarty_mr_splitter <- function(x, spl_char = "|",
                               categs = NULL,
                               add_count = TRUE)
{
    
    spl <- strsplit(x = x, split = spl_char, fixed = TRUE)
    x_name <- deparse(substitute(x))
    x_name <- sub('^.+\\$', '', x_name)
    if (is.null(categs)){
        categs <- sort(unique(unlist(spl)))
        categs <- categs[!is.na(categs)]
    }

    dummy_list <- lapply(spl, function(x) as.integer(categs %in% x))
    res <- as.data.frame(do.call(rbind, dummy_list))
    names(res) <- paste(x_name, gsub(" +", "_", categs), sep = "_")
    
    if (add_count){
        res$count <- apply(res, 1, sum)
        names(res)[length(names(res))] <- paste0(x_name, '_count')
    }
    
    res
}

