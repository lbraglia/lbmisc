#' Remove accents from a character vector
#'
#' Remove accents from a character.
#'
#' @param string a character vector
#' @export
rm_accents <- function(string) {
    
    ## a
    string <- gsub("[\U00E0|\U00E1|\U00E2|\U00E3|\U00E4|\U00E5]",
                   "\U0061", string)
    
    ## e
    string <- gsub("[\U00E8|\U00E9|\U00EA|\U00EB]",
                   "\U0065", string)      
    ## i
    string <- gsub("[\U00EC|\U00ED|\U00EE|\U00EF]",
                   "\U0069", string)
    ## o
    string <- gsub("[\U00F2|\U00F3|\U00F4|\U00F5|\U00F6]",
                   "\u006F", string)     
    ## u
    string <- gsub("[\U00F9|\U00FA|\U00FB|\U00FC]",
                   "\u0075", string)     
    
    return(string)
    
}

#' Remove spaces from a character vector
#'
#' This function removes spaces (all leading and trailing ones,
#' and unique in-between) from a character vector.
#'
#' @param string a character vector 
#' @export
#' @examples
#' test <- c("  test ", "  mediterranean  sea  ")
#' rm_spaces(test)
rm_spaces <- function(string) {
    ## Starting " "
    string <- gsub("[[:space:]]*$", "", string, perl = TRUE)
    ## Ending " "
    string <- gsub("^[[:space:]]*", "", string, perl = TRUE)
    ## 
    gsub("[[:space:]]+", " ", string, perl = TRUE)
}

#' Remove unprintable chars
#'
#' Remove unprintable chars
#' @param string a character vector
#' @export
rm_unprintable_chars <- function(string) gsub("[\001-\037]", "", string)

#' Preprocess data.frame variable names
#'
#' Function to preprocess variable names useful for a data.frame.  If
#' a char is given a char is correct char is given back, if a
#' data.frame the same data.frame with corrected names.
#'
#' This function was created to make automatic variable name creation
#' from Excel files obtained by others.
#'
#' @param x a data.frame or its (col)names
#' @param trim character length of trimming. If \code{NULL} (default)
#'     trimming is disabled.
#' @param dump_rev print a matrix with the reverse renaming. It can be used
#'     with \code{recode} (back to original names?) or
#'     \code{comment_df} (if original names are to be used as
#'     comments)
#' @export
preprocess_varnames <- function(x = NULL,
                                trim = NULL,
                                dump_rev = FALSE)
{

    ## handling cases
    if (! (is.data.frame(x) || is.character(x)))
        stop("varnames need to be a data.frame or character")

    varnames <- if (is.data.frame(x)) names(x) else x
    original_v <- varnames
    
    ## tolower
    varnames <- tolower(varnames)	
    
    ## rm smarty's |
    varnames <- gsub('\\|', '_', varnames)
    
    ## rm parenthesis
    varnames <- gsub('[\\(\\)]', '', varnames)

    ## Multiple spaces (eg Excel-from) to unique underscore
    varnames <- gsub(" +","_", varnames)

    ## change some math operators to equivalent words ("-" excluded)
    varnames <- gsub("-","_", varnames)
    varnames <- gsub("\\/","_frac_", varnames)
    varnames <- gsub("\\*","_per_", varnames)
    varnames <- gsub("\\+","_plus_", varnames)
    varnames <- gsub("<=","_leq_", varnames)
    varnames <- gsub(">=","_geq_", varnames)
    varnames <- gsub("<","_lt_", varnames)
    varnames <- gsub(">","_gt_", varnames)
    
    ## other math related things
    varnames <- gsub("\\%","_perc_", varnames)

    ## R language specificities
    varnames <- gsub(":", "_", varnames)
        
    ## dot to underscore
    varnames <- gsub("\\.","_", varnames)

    ## comma to underscore
    varnames <- gsub(",","_", varnames)

    ## rm accents
    varnames <- rm_accents(varnames)
    
    ## rm unprintable chars
    varnames <- rm_unprintable_chars(varnames)
    
    ## rm other annoying chars
    varnames <- gsub("'", "", varnames)
    varnames <- gsub("\U00B0", "", varnames)

    ## remove starting or ending '_'
    varnames <- gsub("_+$", "", varnames)
    varnames <- gsub("^_+", "", varnames)
    ## unique remaining multiple/near '_'
    varnames <- gsub("_+","_", varnames)
    
    ## Trim to length specified
    if (!is.null(trim))
        varnames <- strtrim(varnames, trim)

    ## Handle duplicate names adding a progressive id
    if (anyDuplicated(varnames)) {
        message("Several variables with the following names:\n")
        cat(unique(varnames[duplicated(varnames)]), sep = '\n')
        message("\n... adding a trailing progressive id")
        dup_index <- duplicated2(varnames)
        prog_id <- group_prog_id(varnames[dup_index])
        varnames[dup_index] <- paste0(varnames[dup_index], '_',
                                      to_00_char(prog_id))
    }

    if (dump_rev){
        cat('Reverse matrix is:\n\n')
        ## alternate elements from varnames and original_v
        rm_v <- Reduce(f = c, Map(c, as.list(varnames), as.list(original_v)))
        ## c stlye is simple
        cat('matrix(c(')
        index <- 1
        for (vn in rm_v){
            cat(deparse(vn))
            if (index != length(rm_v)) cat(',')
            if (index %% 2 == 1L) cat(' ')
            if (index %% 2 == 0L) cat('\n')
            index <- index + 1
        }
        cat("),\n byrow = TRUE, ncol = 2,\n",
            " dimnames = list(NULL, c('new', 'old')))\n\n",
            sep = '')
    }

    if (is.data.frame(x)) {
        names(x) <- varnames
        x
    } else
        varnames
}

#' Transform an integer vector to a character prefixed by an arbitrary
#' number of 0 digits
#' 
#' Transform an integer vector to a character prefixed by an arbitrary
#' number of 0 digits
#'
#' @param x a numeric vector
#' @param clen length of final character vector
#' @examples
#' to_00_char(c(1L, 3L, 20L), 3)
#' to_00_char(c(1L, 3L, 20L), 2)
#' \dontrun{
#' to_00_char(c(1L, 3L, 20L), 2)
#' }
#' @export
to_00_char <- function(x, clen = NULL){
    if(!is.integer(x))
        warning('x should be an integer')
    x <- as.character(as.integer(x))
    xlen <- nchar(x)
    zerolen <- clen - xlen
    if(any(zerolen < 0))
        stop('Some values have length > of clen')
    zeros <- unlist(lapply(zerolen, function(x) {
        paste(rep(0, x), collapse = "")
    }))
    rval <- paste(zeros, x, sep = "")
    rval
}
