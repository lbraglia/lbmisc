#' cicero: some similarities with moustache's idea but designed to
#' be LaTeX friendly and tiny
#'
#' @param tmpl template with \code{<<>>} as delimiters
#' @param strict data provided must have all the same length
#'     (otherwise recycling will act)
#' @param ... data (vectors) passed with the same name as the variable
#'     to be filled in tmpl. If missing lookup is made in the calling
#'     environment
#' @examples
#' 
#' tmp <- "Hi my name's  <<nome>> \textbf{<<cognome>>}.
#' Born on <<dat_nas>>. Heigth <<heigth>>, weight <<weight>>."
#' cicero(tmpl = tmp,
#'        nome = "mario",
#'        heigth = 1.78,
#'        cognome = "rossi",
#'        dat_nas = '2013-10-10',
#'        weight = 80)
#' 
#' nome <- "mario"
#' cognome <- c("rossi", "bianchi")
#' heigth <- 1.78
#' dat_nas <- '2013-10-10'
#' weight <- 80
#' cicero(tmp, strict = FALSE)
#' \dontrun{cicero(tmp, strict = TRUE)}
#' 
#' @export
cicero <- function (tmpl = NULL, strict = FALSE, ...){
    ## template handling: extract tags, varnames and prepare sprintf string
    tmpl <- gsub('\n', '', tmpl)
    tags <- unlist(regmatches(tmpl, gregexpr("(<<.*?>>)", tmpl)))
    varnames <- rm_spaces(gsub("[<>]", "", tags))
    sprintf_str <- gsub('<<.*?>>', '%s', tmpl)
    ## data handling: if no data were specified on the command line
    ## take from the parent frame
    arglist <- list(...)
    if (length(arglist) == 0) arglist <- as.list(parent.frame(n = 2))
    ## check what is missing yet
    missing_args <- setdiff(varnames, names(arglist))
    if (length(missing_args) > 0) {
        msg <- c('Missing arguments for the current template: ',
                 paste(missing_args, collapse = ', '))
        stop(msg)
    }
    ## put the passed arguments in the proper order
    arglist <- arglist[varnames]
    ## check if data in ... or parent.frame have the same length
    arglength <- unlist(lapply(arglist, length))
    not_same_length <- ! all(arglength == arglength[1])
    if (strict && not_same_length)
        stop("Data have not the same length")
    arglist <- lapply(arglist, as.character)
    do.call(sprintf, c(list(sprintf_str), arglist))
}


