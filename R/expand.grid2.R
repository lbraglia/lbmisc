#' A sensible expand.grid
#'
#' An expand.grid with better looking ordering and some defaults
#' (to date, stringsAsFactors = FALSE)
#' 
#' @param ... params given to 
#' @export
expand.grid2 <- function(...){
    dots <- list(...)
    defaults <- list(stringsAsFactors = FALSE)
    dots <- dots[names(dots) %without% names(defaults)]
    params <- c(rev(dots), defaults)
    rval <- do.call(expand.grid, args = params)
    cols <- seq_len(ncol(rval))
    rval[rev(cols)]
}
