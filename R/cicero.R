#' Cicero: some similarities with moustache's idea but designed to be
#' LaTeX friendly, tiny and to allow mixing a bit of R code with templates
#' (if x is a function)
#'
#' @param x template with \code{<<>>} as delimiters, or a function
#' @param strict data provided must have all the same length
#'     (otherwise recycling will act)
#' @param ... data (vectors) passed with the same name as the variable
#'     to be filled in tmpl. If missing lookup is made in the calling
#'     environment
#' @examples
#'
#' ## Example with character template
#' tmp <- "Hi my name's  <<name>> \textbf{<<surname>>}.
#' Born on <<birthdate>>. Heigth <<heigth>>, weight <<weight>>."
#' cicero(x = tmp,
#'         name = "mario",
#'         heigth = 1.78,
#'         surname = "rossi",
#'         birthdate = '2013-10-10',
#'         weight = 80)
#' 
#' name <- "mario"
#' surname <- c("rossi", "bianchi")
#' heigth <- 1.78
#' birthdate <- '2013-10-10'
#' weight <- 80
#' cicero(tmp, strict = FALSE)
#' \dontrun{cicero(tmp, strict = TRUE)}
#'
#' ## Example with function as x
#' ## a possible application for function argument: choose the
#' ## template in runtime and fill it with proper data.
#' select <- function(name, surname, height, weight)
#' {
#'     tmpl1 <- "Hi, I'm  <<name>> <<surname>>. My bmi is ok: <<bmi>>."
#'     tmpl2 <- "Hi, I'm  <<name>> <<surname>>. My bmi is not ok: <<bmi>>."
#'     bmi <- weight/(height^2)
#'     if (bmi < 25) cicero(x = tmpl1, name = name, surname = surname, bmi = bmi)
#'     else cicero(x = tmpl2, name = name, surname = surname, bmi = bmi)
#' }
#'     
#' cicero(x = select,
#'         name = "mario", surname = "rossi",
#'         height = 1.78, weight = 82)
#' 
#' cicero(x = select,
#'         name = "mario", surname = "rossi",
#'         height = 1.78, weight = 75)
#'
#' ## cicero handles functions with argument ... as well
#' f_with_dots <- function(namefirst, ...){
#'     arglist <- list(...)
#'     res1 <- do.call(paste, arglist)
#'     res2 <- do.call(paste, rev(arglist))
#'     if (namefirst) res1 else res2
#' }
#' cicero(x = f_with_dots, namefirst = TRUE, name = "mario", surname = "rossi")
#' cicero(x = f_with_dots, namefirst = FALSE, name = "mario", surname = "rossi")
#'
#'@export
cicero <- function (x = NULL, strict = FALSE, ...){

    xchar <- is.character(x)
    xfun  <- is.function(x)
    if (! (xchar || xfun)) stop('x must be a character or a function')
    ## data/parameters handling: if no data were specified on the
    ## command line take from the parent frame
    arglist <- list(...)
    if (length(arglist) == 0) arglist <- as.list(parent.frame(n = 2))

    ## template handling: extract tags, varnames and prepare sprintf string
    if (xchar){
        x <- gsub('\n', '', x)
        tags <- unlist(regmatches(x, gregexpr("(<<.*?>>)", x)))
        varnames <- rm_spaces(gsub("[<>]", "", tags))
        sprintf_str <- gsub('<<.*?>>', '%s', x)
    }

    if (xfun){
        varnames <- names(formals(x))
        has_dots <- "..." %in% varnames
        if (has_dots) varnames <- varnames %without% "..."
    }

    ## check missing arguments
    missing_args <- setdiff(varnames, names(arglist))
    if (length(missing_args) > 0) {
        msg <- c('Missing arguments for the passed template/function: ',
                 paste(missing_args, collapse = ', '))
        stop(msg)
    }
    ## put the passed arguments in the proper order
    arglist_sel <- arglist[varnames]
    ## if x it's a function and has dots put all the non
    ## matched arguments in ...
    if (xfun && has_dots){
        dots <- arglist[setdiff(names(arglist), varnames)]
        arglist_sel <- c(arglist_sel, dots)
    }
    
    ## check if data in ... or parent.frame have the same length
    if (xchar) {
        arglength <- unlist(lapply(arglist_sel, length))
        not_same_length <- ! all(arglength == arglength[1])
        if (strict && not_same_length)
            stop("Data have not the same length")
    }

    if (xchar) {
        arglist_sel <- lapply(arglist_sel, as.character)
        do.call(sprintf, c(list(sprintf_str), arglist_sel))
    } else if (xfun) {
        do.call(x, arglist_sel)
    }
}


## cicero: some similarities with moustache's idea but designed to
## be LaTeX friendly and tiny
##
## @param tmpl template with \code{<<>>} as delimiters
## @param strict data provided must have all the same length
##     (otherwise recycling will act)
## @param ... data (vectors) passed with the same name as the variable
##     to be filled in tmpl. If missing lookup is made in the calling
##     environment
## @examples
## 
## tmp <- "Hi my name's  <<nome>> \textbf{<<cognome>>}.
## Born on <<dat_nas>>. Heigth <<heigth>>, weight <<weight>>."
## cicero(tmpl = tmp,
##        nome = "mario",
##        heigth = 1.78,
##        cognome = "rossi",
##        dat_nas = '2013-10-10',
##        weight = 80)
## 
## nome <- "mario"
## cognome <- c("rossi", "bianchi")
## heigth <- 1.78
## dat_nas <- '2013-10-10'
## weight <- 80
## cicero(tmp, strict = FALSE)
## \dontrun{cicero(tmp, strict = TRUE)}
## 
## cicero_old <- function (tmpl = NULL, strict = FALSE, ...){
##    ## template handling: extract tags, varnames and prepare sprintf string
##    tmpl <- gsub('\n', '', tmpl)
##    tags <- unlist(regmatches(tmpl, gregexpr("(<<.*?>>)", tmpl)))
##    varnames <- rm_spaces(gsub("[<>]", "", tags))
##    sprintf_str <- gsub('<<.*?>>', '%s', tmpl)
##    ## data handling: if no data were specified on the command line
##    ## take from the parent frame
##    arglist <- list(...)
##    if (length(arglist) == 0) arglist <- as.list(parent.frame(n = 2))
##    ## check what is missing yet
##    missing_args <- setdiff(varnames, names(arglist))
##    if (length(missing_args) > 0) {
##        msg <- c('Missing arguments for the current template: ',
##                 paste(missing_args, collapse = ', '))
##        stop(msg)
##    }
##    ## put the passed arguments in the proper order
##    arglist <- arglist[varnames]
##    ## check if data in ... or parent.frame have the same length
##    arglength <- unlist(lapply(arglist, length))
##    not_same_length <- ! all(arglength == arglength[1])
##    if (strict && not_same_length)
##        stop("Data have not the same length")
##    arglist <- lapply(arglist, as.character)
##    do.call(sprintf, c(list(sprintf_str), arglist))
## }

