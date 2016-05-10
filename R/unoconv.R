#' Unoconv file converter from R
#'
#' Unoconv file converter from R
#' 
#' @param ... file path to be exported
#' @param format file format
#' 
#' @export
unoconv <- function(... , format = stop("Format needed")) {
  Unoconv <- Sys.which("unoconv")
  files <- do.call(paste, list(...))
  system(command = paste(Unoconv, " -f ", format, files) )
}
