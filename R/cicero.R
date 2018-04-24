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
#' \dontrun{
#' cicero(x = tmp,
#'        strict = TRUE,
#'         name = "mario",
#'         heigth = c(1.78, 1.79),
#'         surname = "rossi",
#'         birthdate = '2013-10-10',
#'         weight = 80)
#' }
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
cicero <- function(x, ...) UseMethod('cicero')

#'@export
cicero.character <- function(x, strict = FALSE, ...){

    arglist <- list(...)

    ## extract info from the string passed
    x <- gsub('\n', '', x)
    tags <- unlist(regmatches(x, gregexpr("(<<.*?>>)", x)))
    varnames <- rm_spaces(gsub("[<>]", "", tags))
    sprintf_str <- gsub('<<.*?>>', '%s', x)

    ## check missing arguments, not specified, not default (for function only)
    missing_args <- setdiff(varnames, names(arglist))
    if (length(missing_args) > 0) {
        msg <- c('Missing arguments for the passed template/function: ',
                 paste(missing_args, collapse = ', '))
        stop(msg)
    }

    ## select the available arguments passed at the command line
    arglist_sel <- arglist[names(arglist) %in% varnames]

    ## check arguments length and if not consistent (and it's strict) stop
    arglength <- unlist(lapply(arglist_sel, length))
    not_same_length <- ! all(arglength == arglength[1])
    if (strict && not_same_length) stop("Data have not the same length")

    ## coerce, put in the proper order and sprintf
    arglist_sel <- lapply(arglist_sel, as.character)
    arglist_sel <- arglist_sel[varnames]
    do.call(sprintf, c(list(sprintf_str), arglist_sel))
}


#'@export
cicero.function <- function(x, ...){

    arglist <- list(...)

    ## check the function
    xargs <- formals(x)
    varnames <- names(xargs)
    ## function parameter with defaults
    x_def_args <- Filter(function(arg) !is.symbol(arg), xargs)
    ## dots in function definition
    has_dots <- "..." %in% varnames
    if (has_dots) varnames <- varnames %without% "..."

    ## check missing arguments, not specified, not default
    missing_args <- setdiff(varnames %without% names(x_def_args),
                            names(arglist))
    if (length(missing_args) > 0) {
        msg <- c('Missing arguments for the passed template/function: ',
                 paste(missing_args, collapse = ', '))
        stop(msg)
    }

    ## select the available arguments passed at the command line
    arglist_sel <- arglist[names(arglist) %in% varnames]

    ## add useful defaults from the function which are not
    ## specified in call
    useful_defaults <- names(x_def_args) %nin% names(arglist)
    arglist_sel <- c(arglist_sel, x_def_args[useful_defaults])

    ## if has dots put all the non matched arguments in ...
    if (has_dots){
        dots <- arglist[setdiff(names(arglist), varnames)]
        arglist_sel <- c(arglist_sel, dots)
    }

    ## make the call
    do.call(x, arglist_sel)
}
