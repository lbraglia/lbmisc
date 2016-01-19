#' update package shortcut
#'
#' update package shortcut with my defaults
#' @param ask same as \code{\link{update.packages}}
#' @export 
update_packages <- function(ask = FALSE) {
  personal_library <- grep(path.expand("~/R"), .libPaths(), value = TRUE)
  update.packages(lib.loc = personal_library ,
                  repos = "http://cran.rstudio.com",
                  ask = ask)
}
