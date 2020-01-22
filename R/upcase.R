#' Capitalize words in a vector of characters
#'
#' Capitalize words in a vector of characters
#' 
#' @param s vector of characters
#' @param method either sentence (first letter in each element of the
#'     vector), word (first letter in each word of the sentence) or strict (
#'     first letter in each word of the sentence)
#' @examples
#' upcase(c("using AIC", "for model selection"))
#' upcase(c("using AIC", "for model selection"), method = 'word')
#' upcase(c("using AIC", "for model selection"), method = 'strict')
#'@export
upcase <- function(s, method = c('sentence', 'word', 'strict')){
    method <- match.arg(method)
    if (method == 'sentence') {
        paste0(toupper(substring(s, 1, 1)), substring(s, 2))
    } else {
        ## thanks to capwords from ?gsub
        strict <- method %in% 'strict'
        upcase_worker <- function(s) {
            paste(
                toupper(substring(s, 1, 1)),
                {s <- substring(s, 2); if(strict) tolower(s) else s},
                sep = "", collapse = " "
            )
        }
        sapply(strsplit(s, split = " "),
               upcase_worker,
               USE.NAMES = !is.null(names(s)))
    }
}
