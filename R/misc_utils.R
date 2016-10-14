#' update package shortcut
#'
#' update package shortcut with my defaults
#' @param ask same as \code{\link{update.packages}}
#' @export 
update_packages <- function(ask = FALSE) {

  personal_library <- grep(path.expand("~/R"), .libPaths(), value = TRUE)
  utils::update.packages(lib.loc = personal_library ,
                  repos = "http://cran.rstudio.com",
                  ask = ask)
}

#' read R news
#'
#' read R news
#' @param ... arguments passed to \code{\link{news}}
#' @export
rnews <- function(...) utils::page(utils::news(...), method = 'print')

#' handy R version
#'
#' easier access to R version
#' 
#' @export
r_version <- function() paste(R.version$major, R.version$minor, sep = '.')
