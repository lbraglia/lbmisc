#' menu-like functions for non interactive sections
#'
#' @param choiches vector of possible choiches
#' @param title give a title?
#' @param allow_multiple allows multiple choices
#' @export
menu_ni <- function(choices, title = NULL, allow_multiple = FALSE){
    available <- seq_along(choices)
    the_menu <- paste(available, choices, sep = '. ', collapse = "\n")
    avvertenza <- "a) Insert values separed by ' '; b) [ENTER]; c) [Ctrl+D]"
    if (!is.null(title)) cat(title, "\n\n")
    cat(the_menu,   '\n')
    cat(avvertenza, '\n')
    con <- if (interactive()) stdin() else file('stdin')
    res <- as.integer(strsplit(readLines(con = con, n = 1), ' ')[[1]])
    res <- if (allow_multiple) res else res[1]
    while (!all(res %in% available)){
        cat("Not valid insertion!\n")
        cat(avvertenza, '\n')
        ## res <- as.integer(scan(file = 'stdin', character(), n = 1))
        res <- as.integer(strsplit(readLines(con = con, n = 1), ' ')[[1]])
        res <- if (allow_multiple) res else res[1]
    }
    choices[res]
}
