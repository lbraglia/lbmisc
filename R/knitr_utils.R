#' knitr default pretty printer
#'
#' knitr default pretty printer
#' @param x R object to be printed
#' @examples
#'
#' ## library(knitr)
#' ## opts_chunk$set("engine" = "R")
#' ## knit_hooks$set(inline = lbmisc::knitr_inline)
#' 
#' @export
knitr_inline <- function(x){
    if (is.double(x)) sprintf('%.2f',x)
    else if (is.integer(x)) sprintf('%d',x)
    else if (is.character(x)) sprintf('%s',x)
}
