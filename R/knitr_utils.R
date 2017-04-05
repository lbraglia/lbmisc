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

#' pretty print confidence interval from a vector
#'
#' pretty print a confidence interval from a vector of three values
#' (first: estimate, second: lower ci, third: upper ci) in a latex
#' document
#' @param x the length-3 vector
#' @param type is it a numerics, integers or percentages vector
#' @param measurement_unit used string for measurement unit
#' @param digits number of digits displayed
#' @param conf_lev_string used string for confidence level
#' @param style \code{brack_est_ci_brack} puts estimate and ci between
#'     brackets, while \code{est_brack_ci_brack} put the estimate out of them
#' @param display_brackets whether to print bracket or not
#' 
#' @export
latex_ci <- function(x, 
                     type = c('num', 'int', 'perc'), 
                     measurement_unit = '',
                     digits = 1,
                     conf_lev_string = '95\\% CI',
                     style = c('brack_est_ci_brack', 'est_brack_ci_brack'),
                     display_brackets = TRUE){

    type <- match.arg(type)
    style <- match.arg(style)

    if (type == 'perc' && measurement_unit != ''){
        warning("Percentages can't have measurement unit. Latter ignored")
        measurement_unit <- ''
    }
    
    format <- paste0('%.', 
                     digits, 
                     ## choose format letter
                     if (type %in% c('num', 'perc')) 'f' 
                     else if (type == 'int') 'd'
                     else stop("type not handled"),
                     ## add percentage
                     if (type %in% 'perc') '\\%%' else '')

    ## add unit of measurement for the estimate
    est_format <- paste0(format, 
                         if (measurement_unit != '')
                             paste0(' ', measurement_unit)
                         else '')
    
    est    <- sprintf(est_format, x[1])
    low_ci <- sprintf(format,     x[2])
    up_ci  <- sprintf(format,     x[3])

    if (display_brackets){
        start_brack <- '('
        end_brack <- ')'
    } else {
        start_brack <- end_brack <- ''
    }

    if (style == 'brack_est_ci_brack') 
        paste0(start_brack, est, ', ', conf_lev_string , 
               ' ', low_ci, ' - ', up_ci, end_brack)
    else if (style == 'est_brack_ci_brack') 
        paste0(est, start_brack, conf_lev_string , 
               ' ', low_ci, ' - ', up_ci, end_brack)
}
