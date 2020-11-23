#' print a simple ascii header
#'
#' @param x what to print as title
#'@export
ascii_header <- function(x){
    x <- toupper(x)
    header <- paste(rep("=", nchar(x)), collapse = '')
    cat(header, x, header, '', sep = '\n')
}
