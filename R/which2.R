
#' which.min who handles all NA returning NA
#' 
#' which.min who handles all NA returning NA
#'
#' @param x a vector
#' @export
which.min2 <- function(x){
    if (all(is.na(x))) NA  else which.min(x)
}

#' which.max who handles all NA returning NA
#' 
#' which.max who handles all NA returning NA
#'
#' @param x a vector
#' @export
which.max2 <- function(x){
    if (all(is.na(x))) NA  else which.max(x)
}
