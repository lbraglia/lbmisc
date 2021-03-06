#' Report NA variables per case
#'
#' Report NA variables per case
#' @param x data.frame to be analyzed
#' @param id_var variables to be used as id (characters from
#' name(data.farme)
#' @examples
#' test <- data.frame(
#'  name =  c("john","mary","gregor"),
#'  x = c(1,NA,NA),
#'  y = c(NA,1,1),
#'  z = c(1,1,NA))
#' test2 <- test
#' test2$surname <- c("doe", "foo", "bar")
#' test2 <- test2[c("name","surname","x","y","z")]
#'
#' test
#' NA_report(test, "name")
#'
#' test2
#' NA_report(test2, c("name", "surname"))
#' @export
NA_report <- function(x, id_var = NULL)
{
    ## input checking
    if (!is.data.frame(x))
        stop("x must be a data.frame")
    if (! (is.character(id_var) & length(id_var)>0L) )
        stop("id_var must be a positive length character vector")
    if (anyDuplicated(x[, id_var]))
        stop("'id_var' must identify rows uniquely")
    ## multiple ids handling
    if (length(id_var) == 1L)
        ID <-  x[, id_var]
    else {
        ID <- do.call("paste", c(x[, id_var], sep = "\r"))
        idDF <- cbind(x[, id_var], ID)
    }
    ## splitting per id (list of one-liners)
    splitted <- split(x[, names(x) %without% id_var], f = factor(ID))
    ## Missing variable per units
    nas <- lapply(splitted, function(x) names(x)[is.na(x)])
    ## Creating a similar data structure for id
    ids <- mapply(FUN = rep, names(nas), lapply(nas, length))
    ## putting all together
    res <- as.data.frame(do.call("rbind", mapply(cbind, ids, nas)))
    ## back to original id (not pasted one) for multiple id columns
    ## data.frame
    if (length(id_var) > 1L){
        res <- merge(idDF, res, by.x = "ID", by.y = "V1")
        res$ID <- NULL
    }
    names(res) <- c(id_var, "variable")
    return(res)
}

#' remove NA and make a message with number of units deleted
#'
#' like na.omit but print a message
#' @param x something handled by \code{stats::na.omit}
#' @param ... other options passed to proper methods
#' @examples
#' nona <- NA_remove(data.frame(a = 1:6,
#'                              b = c(NA, 3, NA, 4, 5, 6),
#'                              c = c(rep(NA, 4), 'a', 'b')))
#' 
#' @export
NA_remove <- function(x, ...) UseMethod('NA_remove')

#' @export
NA_remove.default <- function(x, ...) stop('Not implemented')

#' @export
NA_remove.data.frame <- function(x, quiet = FALSE, ...){
    y  <- stats::na.omit(x)
    if (!quiet){
        nx <- nrow(x)
        ny <- nrow(y)
        if (nx != ny){
            miss <- unlist(lapply(x, function(x) sum(is.na(x))))
            miss <- sort(miss[miss > 0], decreasing = TRUE)
            miss_string <- paste(sprintf("%s = %d", names(miss), miss),
                                 sep  = '', collapse = '\n')
            message('Rows were ' , nx, ', now are ', ny, '. ',
                    nx - ny,
                    if (nx - ny > 1) ' rows ' else ' row ',
                    'deleted due missingness.\n',
                    'Missing frequencies by variable (decreasing order):\n\n',
                    miss_string,
                    '\n')


        }
    }
    return(y)
}


#' plot patterns of missing values
#'
#' plot patterns of missing values
#' @param x a data.frame or a matrix
#' @param ... other options passed to proper methods
#' @examples
#' 
#' ## NA_plot(airquality)
#' ## NA_plot(. ~ Month, data = airquality)
#' @export
NA_plot <- function(x, ...) UseMethod('NA_plot')
#' @export
NA_plot.default <- function(x, ...) stop('Not implemented')
#' @export
NA_plot.data.frame <- function(x, ...) {
    x <- as.matrix(x)
    NA_plot.matrix(x)
}

#' @export
NA_plot.matrix <- function(x, ...) {
    x <- t(is.na(x))
    lattice::levelplot(x = x,
                       aspect = 'fill',
                       col.regions = gray(c(0.9, 0.1)),
                       ylab = 'row',
                       xlab = 'Variable',
                       main = 'Missing data patterns',
                       colorkey = FALSE)
}
