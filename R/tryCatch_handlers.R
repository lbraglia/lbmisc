tryCatch_handler <- function(x,
                             rval = invisible(NULL),
                             type = c('Error', 'Warning', 'Message'),
                             verbose = TRUE)
{
    type <- match.arg(type)
    if (verbose) cat(type, ":", x$message, "\n")
    rval
}

#partial the previous function

#'@export
tryCatch_error <- function(x, rval = invisible(NULL), verbose = TRUE)
    tryCatch_handler(x = x, rval = rval, type = 'Error', verbose = verbose)


#'@export
tryCatch_warning <- function(x, rval = invisible(NULL), verbose = TRUE)
    tryCatch_handler(x = x, rval = rval, type = 'Warning', verbose = verbose)

#'@export
tryCatch_message <- function(x, rval = invisible(NULL), verbose = TRUE)
    tryCatch_handler(x = x, rval = rval, type = 'Message', verbose = verbose)

