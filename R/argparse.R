#' Simple command line argument parser
#'
#' Simple command line argument parser: returns a named list with
#' arguments and values
#' 
#' @examples
#' ## Beginning of an Rscript
#' #!/usr/bin/Rscript --vanilla --quiet
#' args <- lbmisc::argparse()
#' @export
argparse <- function(){
    line <- commandArgs(trailingOnly = TRUE)
    ## nome argomenti
    arg_pos <- grepl('^--', line) | grepl('^-', line)
    arg_names <- gsub("^-+", "", line[arg_pos])
    ## valore degli argomenti: splitto ad inizio argomento e tolgo il nome
    arg_values <- split(line, cumsum(arg_pos))
    arg_values <- lapply(arg_values, function(x) x[-1])
    ## return
    setNames(arg_values, arg_names)
}
