#' Calculate age in years
#'
#' Calculate age in years
#' @param birth date of birth
#' @param date date used to calculate the age
#' @examples
#' @export
age <- function(birth, date){
    birth <- as.Date(birth)
    date <- as.Date(date)
    res <- as.integer(date - birth)/365.25
    if (any(res < 0))
        warning('Some ages are < 0.')
    res
}
