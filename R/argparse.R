#' Simple command line argument parser
#'
#' Simple command line argument parser: returns a named list with
#' arguments and values
#' @param opts a list of lists, each of one has tree argument: 'name'
#'     (a character eg 'example' is for --example), 'default' (a value
#'     if nothing is specified at command line, eg TRUE), 'f' (the
#'     coercer function to assure the input is validated and of the
#'     proper type)
#' @examples
#' ## Beginning of an Rscript
#' #!/usr/bin/Rscript --vanilla --quiet
#'
#' # No opts, no validation
#' args <- lbmisc::argparse()
#'
#' # With opts, validation
#' opts <- list(
#'    list(name = 'a_character' , default = "hello" , f = as.character),
#'    list(name = 'a_logical'   , default = "TRUE"  , f = as.logical),
#'    list(name = 'an_integer'  , default = "1L"    , f = as.integer),
#'    list(name = 'a_numeric'   , default = "1.5"   , f = as.numeric)
#' )
#' args <- lbmisc::argparse(opts = opts)
#' ## print the effect of validation
#' print(str(args))
#' 
#' ## with from the command line
#' ## rscript --a_character --a_logical FALSE --an_integer 2 --a_numeric
#' 
#' @export
argparse <- function(line = commandArgs(trailingOnly = TRUE),
                     opts = NULL){
    ## nome argomenti
    arg_pos <- grepl('^--', line) | grepl('^-', line)
    arg_names <- gsub("^-+", "", line[arg_pos])
    ## valore degli argomenti: splitto ad inizio argomento e tolgo il nome
    arg_values <- split(line, cumsum(arg_pos))
    arg_values <- lapply(arg_values, function(x) x[-1])
    ## return
    arg_values <- setNames(arg_values, arg_names)

    ## output validation
    if (is.null(opts)) arg_values
    else {
        valid_names <- unlist(lapply(opts, function(x) x$name))
        ## keep the valid input
        arg_values <- arg_values[names(arg_values) %in% valid_names]
        missing_names <- valid_names %without% names(arg_values)
        ## add the default missing
        arg_values[missing_names] <- NA
        normalizer <- function(args, opts){
            if (length(args) == 0L || is.na(args)) opts$f(opts$default) else opts$f(args)
        }
        ## put in the same order
        arg_values <- arg_values[valid_names]
        arg_values <- Map(normalizer, arg_values, opts) 
        arg_values
    }
}
