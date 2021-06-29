#' update package shortcut
#'
#' update package shortcut with my defaults
#' @param ask same as \code{\link{update.packages}}
#' @export 
update_packages <- function(ask = FALSE) {

  personal_library <- grep(path.expand("~/R"), .libPaths(), value = TRUE)
  utils::update.packages(lib.loc = personal_library ,
                         ## repos = "http://cran.rstudio.com",
                         repos = 'http://cloud.r-project.org',
                         ask = ask)
}

#' read R news
#'
#' read R news
#' @param ... arguments passed to \code{\link{news}}
#' @export
r_news <- function(...) utils::page(utils::news(...), method = 'print')

#' handy R version
#'
#' easier access to R version
#' @examples
#' r_version()
#' @export
r_version <- function() paste(R.version$major, R.version$minor, sep = '.')

#' Save a named list (eg a list of data.frames from lbmisc::import) in 
#' the global environment
#' 
#' @param x a named list passed to list2env
#' @param ... other parameters passed to list2env
#' @export
list2globalenv <- function(x, ...) {
    list2env(x, envir = globalenv(), ...)
    invisible(NULL)
}
