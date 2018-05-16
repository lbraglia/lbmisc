#' Find the start and end of enrollment
#'
#' Find the start and end of enrollment and report in month year char format
#' 
#' @param x a Date of patient enrollment
#' @param lang output language
#' 
#' @export
enrollment_period <- function(x, lang = c('en', 'it')){# enrollment date
    lang <- match.arg(lang)
    lct <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", lct))
    start <- min(x, na.rm = TRUE)
    end <- max(x, na.rm = TRUE)
    frmt <- '%B %Y'
    Sys.setlocale("LC_TIME", if ('it' == lang) "it_IT.UTF-8" else "C")
    c("from" = format(start, frmt), "to" = format(end, frmt))
}
