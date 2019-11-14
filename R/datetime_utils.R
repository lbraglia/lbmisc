#' Months in italian, the easy way
#'
#' Months in italian, the easy way
#' 
#' @export
mesi <- c('Gennaio', 'Febbraio', 'Marzo', 'Aprile', 'Maggio', 'Giugno',
          'Luglio', 'Agosto', 'Settembre', 'Ottobre', 'Novembre',
          'Dicembre')

#' Abbreviated months in italian, the easy way
#'
#' Abbreviated months in italian, the easy way
#' 
#' @export
mesi.abb <- c('Gen', 'Feb', 'Mar', 'Apr', 'Mag', 'Giu',
              'Lug', 'Ago', 'Set', 'Ott', 'Nov', 'Dic')


#' test if a vector is a date
#' 
#' @param x a vector
#' @export
#' 
is.Date <- function(x) inherits(x, 'Date')


#' Create a \code{\link{Date}} from month, day and year.
#' 
#' Create a \code{\link{Date}} from month, day and year.
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
    if (! inherits(x, 'Date'))
        stop('x must be a Date.')
    x <- x + days
    x <- as.POSIXlt(x)
    x$year <- x$year + years
    x$mon <- x$mon + months
    as.Date(x)
}

#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @param x Date, POSIXct or POSIXlt vector
#' @return A numeric vector.
#' @examples
#' 
#' Sys.Date()
#' year(Sys.Date())
#' year(as.POSIXct(Sys.Date()))
#' year(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export year
year <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("year")
 
}

#' @export 
year.Date <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @export 
year.POSIXct <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @export 
year.POSIXlt <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @param x Date, POSIXct or POSIXlt vector
#' @param string Logical, if TRUE months string is returned, otherwise numeric
#' @param abbreviate logical. Should the months in string format be
#' abbreviated?
#' @return A vector with extracted months.
#' @examples
#' 
#' Sys.Date()
#' month(Sys.Date())
#' month(Sys.Date(), string = FALSE)
#' month(Sys.Date(), string = TRUE, abbreviate = TRUE)
#' ## from base package
#' month(Sys.Date())
#' month(as.POSIXct(Sys.Date()))
#' month(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export month
month <- function(x, string = TRUE, abbreviate = FALSE) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("month")
 
}

#' @export 
month.Date <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x = x, if (abbreviate) "%b" else "%B"))
    }
    
}

#' @export 
month.POSIXct <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x = x, if (abbreviate) "%b" else "%B"))
    }
    
}

#' @export
month.POSIXlt <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x = x, if (abbreviate) "%b" else "%B"))
    }
    
}

#' Extract numeric days of month from a \code{Date},
#' \code{POSIXct} or \code{POSIXlt} vector.
#' 
#' Extract numeric days of month from a \code{Date},
#' \code{\link{POSIXct}} or \code{\link{POSIXlt}} vector. 
#' 
#' @param x Date, \code{\link{POSIXct}} or \code{\link{POSIXlt}}
#' vector
#' @return A numeric vector.
#' @examples
#' 
#' Sys.Date()
#' day(Sys.Date())
#' day(as.POSIXct(Sys.Date()))
#' day(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export day
day <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("day")
 
}


#' @export
day.Date <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export 
day.POSIXct <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export 
day.POSIXlt <- function(x) {
    as.numeric(format(x, "%d"))
}


#' Quick alias for importing italian csv common date formats
#' 
#' @param x character with date
#' @param fmt format given to as.Date
#' @param ... other arguments passed to as.Date
#' 
#' @export
dmy_import <- function(x, fmt = "%d/%m/%Y", ...){
    as.Date(x, format = fmt, ...)
}


#' Quick alias for importing italian csv common datetime
#' 
#' @param x character with date
#' @param fmt format given to as.Date
#' @param output class name of the ouput
#' @param ... other arguments passed to as.POSIXlt
#' 
#' @export
datetime_import <- function(x, 
                            fmt = "%d/%m/%Y %H.%M.%S",
                            output = c("Date", "POSIXlt", "POSIXct"),
                            ...)
{
    output <- match.arg(output)
    posix_lt <- as.POSIXlt(x, format = fmt, ...)
    if (output == 'Date') 
        as.Date(posix_lt) 
    else if (output == 'POSIXlt')
        posix_lt
    else
        as.POSIXct(posix_lt)
}
