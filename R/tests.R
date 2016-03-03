#' Vector testing for humans (tm)
#'
#' returns TRUE only if x is atomic, not NULL, and with NULL
#' dim. It should be useful in function programming, in order to check
#' parameter (are they vector in the common sense meaning of the word?)
#' 
#' @param x object to be tested
#' @export
#' @examples
#' is.not_null_vector(logical())
#' is.not_null_vector(integer())
#' is.not_null_vector(numeric())
#' is.not_null_vector(complex())
#' is.not_null_vector(character())
#' is.not_null_vector(raw())
#' is.not_null_vector(NULL)
#' is.not_null_vector(matrix(1:10, 2))
#' is.not_null_vector(list())        
#' is.not_null_vector(expression())
#' is.not_null_vector(pairlist())  
#' is.not_null_vector(pairlist(1))
is.not_null_vector <- function(x)
{
    is.atomic(x) && (!is.null(x)) && is.null(dim(x))
}
