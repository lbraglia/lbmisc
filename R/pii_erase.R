#' Remove PII (Personally identifiable information) from a data.frame
#' 
#' @param x data.frame to be anonymized
#' @param quiet if FALSE (by default) emit a message with erased variables
#' 
#'@export
pii_erase <- function(x, quiet = FALSE)

{
    if (!is.data.frame(x)) stop("x must be a data.frame")

    surname     <- varname_index(x, c("cognome", "surname"))
    name        <- varname_index(x, c("nome", "name"))
    mail        <- unlist(lapply(x, is_email))
    fiscal_code <- unlist(lapply(x, is_fiscal_code))
    ## phone       <- unlist(lapply(x, is_phone))
    ## mobile      <- unlist(lapply(x, is_mobile))
    
    removed <-
        surname      |
        name         |
        mail         |
        fiscal_code  ## |
        ## _phone        |
        ## _mobile

    if (!quiet) {
        rm_names <- names(x)[removed]
        message("Likely PII detected, removing:  \n- ",
                paste(rm_names, collapse = "\n- "), "\n")
    }

    x[, !removed]
}

## Function factory for tests searching for at least one match of a
## pattern in a vector
any_match <- function(pattern){function(x) any(grepl(pattern, x))}

## Some products
is_email       <- any_match("^[[:alnum:].-]+@[[:alnum:].-]+$")
is_fiscal_code <- any_match("[A-Z]{6}\\d{2}[A-Z]{1}\\d{2}[A-Z]{1}\\d{3}[A-Z]{1}")
## is_phone       <- any_match("\\d{3}?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}")
## is_mobile      <- any_match("TODO")


## Misc Utils
low_no_spaces <- function(x) lbmisc::rm_spaces(tolower(x))
varname_index <- function(x, varnames){
    nx  <- low_no_spaces(names(x))
    vn  <- low_no_spaces(varnames)
    nx %in% vn
}


pii_cleaner_output <- function(i){



}

#' Import multiple dataset, remove PII and save anonymized
#'
#' @param input importer x param
#' @param outfile exporter x param
#' @export
pii_cleaner <-
    function(input = tcltk::tk_choose.files(caption = "Select DATA FILES to be imported and anonymized"),
             outfile = NULL,
             ...)
{
    dbs <- importer(input, ...)
    dbs <- verbose_lapply(dbs, pii_erase)
    exporter(x = dbs, f = outfile)
}
