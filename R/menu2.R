#' menu-like function for interactive or non-interactive sections
#' which allows multiple choices as well
#'
#' return NA if the user inserted the usual 0 to exit
#'
#' @param choiches vector of possible choiches
#' @param title optional
#' @param multiple can more than one item be selected?
#' @param return what to return values (selected choiches given, by
#'     default), or indexes. If only 0 is selected (to exit), NA is
#'     returned
#' @param strict allow only selectable index to be choosen
#' @export
menu2 <- function(choices, title = NULL, multiple = FALSE,
                  return = c('values', 'indexes'),
                  strict = FALSE)
{
    return <- match.arg(return)
    available_ind <- seq_along(choices)
    avail_with_0 <- c(0, available_ind)
    the_menu <- paste(available_ind, choices, sep = '. ', collapse = "\n")
    interactive <- interactive()
    con <- if (interactive) stdin() else file('stdin')
    selection_msg <- if (interactive){
                         if (multiple)
                             "Selection (values as '1, 2-3, 6') or 0 to exit: "
                         else {
                             "Selection (0 to exit): "
                         }
                     } else {
                         if (multiple){
                             "a) Insert selection (values as '1, 2-3, 6') or 0 to exit; b) [ENTER]; c) [Ctrl+D]\n"
                         } else {
                             "a) Selection (0 to exit); b) [ENTER]; c) [Ctrl+D]\n "
                         }
                     }

    ## get infos from user
    if (!is.null(title)) cat(title, "\n\n")
    cat(the_menu,   '\n\n')
    cat(selection_msg)
    line <- readLines(con = con, n = 1)
    ind <- line_to_numbers(line)
    ind <- if (multiple) ind else ind[1]

    if (strict){
        ## continua a continuare fino a che gli indici sono tutti tra
        ## i selezionabili o 0 per uscire
        while (!all(ind %in% avail_with_0)){
            not_in <- ind[! (ind %in% avail_with_0)]
            cat("Not valid insertion:", not_in, "\n")
            cat(selection_msg)
            line <- readLines(con = con, n = 1)
            ind <- line_to_numbers(line)
            ind <- if (multiple) ind else ind[1]
        }
    } else {
        ## non ciclare ma tieni comunque quello che c'e` di tenibile
        ## indici positivi o nulli nel range
        allowed <- ind %in% avail_with_0
        any_nin_avail <- any(! allowed)
        if (any_nin_avail){
            not_allowed <- ind[!allowed]
            warning("Removed some values (not 0 or specified possibilities:", not_allowed, ".")
            ind <- ind[allowed]
        }
    }

    ## return values or NA if nothing was choosed
    ind <- unique(ind)
    ind <- ind %without% 0
    if (length(ind) > 0) {
        if (return == 'values') choices[ind] else ind
    } else NA 
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
