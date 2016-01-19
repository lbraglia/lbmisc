#' Create a \code{\link{Date}} from month, day and year.
#' 
#' 
#' Create a \code{\link{Date}} from month, day and year.
#' 
#' 
#' @param month Numeric vector of months
#' @param day Numeric vector of days
#' @param year Numeric vector of years
#' @return A vector of \code{\link{Date}}.
#' @examples
#' db <- data.frame(month = 10:12, day = 15:17, year = 1982:1984)
#' date_mdy(month = db$month, day= db$day, year=db$year)
#' @export
date_mdy <- function(month, day, year) {
    as.Date(sprintf("%s-%s-%s", year, month, day))
}	


#' Add a period of time to a Date object
#'
#' Add a period of time to a Date object
#' @param x starting Date
#' @param years years to be added
#' @param months months to be added
#' @param days days to be added
#' @examples
#' (today <- Sys.Date())
#' date_plus(today, years = 1, months = 1, days = 1)
#' @export
date_plus <- function(x, years = 0, months = 0, days = 0){
    x <- as.POSIXlt(x)
    x$year <- x$year + years
    x$mon <- x$mon + months
    x$day <- x$day + days
    as.Date(x)
}
