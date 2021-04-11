#' menu-like function for interactive or non-interactive sections
#' which allows multiple choices as well
#'
#' @param choiches vector of possible choiches
#' @param title optional
#' @param multiple can more than one item be selected?
#' @param return what to return values (selected choiches given, by
#'     default), or indexes
#' @param strict allow only selectable index to be choosen
#' @export
menu2 <- function(choices, title = NULL, multiple = FALSE,
                  return = c('values', 'indexes'),
                  strict = FALSE)
{
    return <- match.arg(return)
    available_ind <- seq_along(choices)
    the_menu <- paste(available_ind, choices, sep = '. ', collapse = "\n")
    interactive <- interactive()
    avvertenza <- "a) Insert values; b) [ENTER]; c) [Ctrl+D]\n"
    if (!is.null(title)) cat(title, "\n\n")
    cat(the_menu,   '\n\n')
    if (!interactive) cat(avvertenza, '\n')
    cat("Selection (values as '1, 2-3, 6'): ")
    con <- if (interactive) stdin() else file('stdin')
    line <- readLines(con = con, n = 1)
    ind <- line_to_numbers(line)
    ind <- if (multiple) ind else ind[1]

    if (strict){
        while (!all(ind %in% available_ind)){
            not_in <- ind[! (ind %in% available_ind)]
            cat("Not valid insertion:", not_in, "\n")
            if (!interactive) cat(avvertenza, '\n')
            cat("Selection (values as '1, 2-3, 6'): ")
            line <- readLines(con = con, n = 1)
            ind <- line_to_numbers(line)
            ind <- if (multiple) ind else ind[1]
        }
    }

    ind <- unique(ind)
    if (return == 'values') choices[ind] else ind
}

line_to_numbers <- function(x){
    ## transform "1 2-3, 4, 6-10" to c(1:3, 4, 6:10)
    x <- gsub(",", " ", x)
    x <- as.list(strsplit(x, " ")[[1]])
    x <- lapply(x, line_to_numbers_worker)
    unlist(x)
}

line_to_numbers_worker <- function(x) {
    if (x == '') {
        NULL
    } else if (grepl("\\d-\\d", x)) {
        first <- gsub("(\\d+)-\\d+" , "\\1", x)
        second <- gsub("\\d+-(\\d+)", "\\1", x)
        seq(from = first, to = second)
    } else{
        as.integer(x)
    }
}

## Todo, fai line to number
## testa in modalita batch e non
