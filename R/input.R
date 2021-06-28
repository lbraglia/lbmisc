#' Obtain input from the user handling batch situation
#' 
#' @param msg title
#' @export
input <- function(msg){
    if (interactive()){
        rval <- readline(msg)
    } else {
        con <- file('stdin')
        cat("Insert the line, then [ENTER][Ctrl+D]\n")
        cat(msg)
        rval <- readLines(con = con, n = 1)
    }
    rval
}
