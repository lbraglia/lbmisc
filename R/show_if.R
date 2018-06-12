#' show a chunk of a report if a logical condition is met
#' 
#' start showing a chunk of a report if a logical condition is met; to
#' be used in a results = 'asis' chunk of Sweave/knitr
#'
#' @param x a logical value
#' @examples
#'
#' eval_old_stuff <- FALSE
#' show_if(eval_old_stuff)
#' 
#' @export
show_if <- function(x) if (!x) cat("\\iffalse")

#' end showing a chunk of a report if a logical condition is met
#' 
#' end showing a chunk of a report if a logical condition is met; to
#' be used in a results = 'asis' chunk of Sweave/knitr
#'
#' @param x a logical value
#' @examples
#'
#' eval_old_stuff <- FALSE
#' end_show_if(eval_old_stuff)
#' 
#' @export
end_show_if <- function(x) if (!x) cat("\\fi")
