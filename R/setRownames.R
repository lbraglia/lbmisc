#' set rownames of an object and return it
#'
#' set rownames of an object and return it
#' @param object an object with rows (where rownames works)
#' @param nm rownames to be assigned
#' 
#'@export
setRownames <- function (object = nm, nm) 
{
    rownames(object) <- nm
    object
}
