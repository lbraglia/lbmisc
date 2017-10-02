#' Convert datetime values from SPSS
#' 
#' Convert the number of seconds since 1582-10-14 available in SPSS datetime
#' to a format specified by coercer
#' 
#' @param x number of seconds since 1582-10-14
#' @param coercer function to use as a coercer given the desidered output
#' 
#' @export
spss_datetime <- function(x, coercer = as.Date)
    coercer(ISOdate(year = 1582, month = 10, day = 14) + x)
