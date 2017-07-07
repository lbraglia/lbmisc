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
smarty_mr_splitter <- function(x,
                               spl_char = "|",
                               categs = NULL,
                               add_count = TRUE)
{

    if (is.data.frame(x)){
        x_name <- names(x)
        x <- x[[1]]
    } else {
        x_name <- deparse(substitute(x))
        x_name <- sub('^.+\\$', '', x_name)
    }

    spl <- strsplit(x = x, split = spl_char, fixed = TRUE)

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


#' Replace smarty multiple responses with proper dummies
#'
#' @param x data.frame of interest
#' @param variables mr variables to be replaced
#' @param drop remove original mr variable once dummies have been added?
#' @export
replace_mr_with_dummies <- function(x = NULL, variables = NULL, drop = FALSE){
    stopifnot(is.data.frame(x), is.character(variables))
    for (variable in variables){
        if (variable %in% names(x)){
            location <- which(names(x) %in% variable)
            tmp <- smarty_mr_splitter(x = x[, variable, drop = FALSE],
                                      add_count = FALSE)
            x <- as.data.frame(append(x = x, values = tmp, after = location))
            if (drop) x[, variable] <- NULL
        }
    }
    x
}
