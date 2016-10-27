#' Vector testing for humans (tm)
#'
#' returns TRUE only if x is atomic, not NULL, and with NULL
#' dim. It should be useful in function programming, in order to check
#' parameter (are they vector in the common sense meaning of the word?)
#' 
#' @param x object to be tested
#' @export
#' @examples
#' elem_list <- list(logical = logical(),
#'                   integer = integer(),
#'                   numeric = numeric(),
#'                   complex = complex(),
#'                   char = character(),
#'                   raw = raw(),
#'                   null = NULL,
#'                   matrix = matrix(1:10, 2),
#'                   list = list(),
#'                   expression = expression(),
#'                   pairlist = pairlist(),  
#'                   pairlist1 = pairlist(1))
#' lapply(elem_list, is.vector2)
is.vector2 <- function(x)
{
    is.atomic(x) && (!is.null(x)) && is.null(dim(x))
}


#' Test if a vector represents a quantitative variable
#'
#' @param x vector to be tested
#' @export
is.quantitative <- function(x)
{
    is.vector2(x) && is.numeric(x) && (!all(x %in% c(NA, 0, 1)))
}

#' Test if a vector represents a percentage variable
#'
#' @param x vector to be tested
#' @export
is.percentage <- function(x)
{
    is.vector2(x) && is.numeric(x) && (all(x %in% c(NA, 0, 1)))
}

#' Test if a vector represents a qualitative variable
#'
#' @param x vector to be tested
#' @export
is.qualitative <- function(x)
{
    is.vector2(x) && is.factor(x)
}

