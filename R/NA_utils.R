#' Report NA variables per case
#'
#' Report NA variables per case
#' @param x data.frame to be analyzed
#' @param id_var variable to be used as id
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
  if (anyDuplicated(x[,id_var]))
    stop("'id_var' must identify rows uniquely")
  ## multiple ids handling
  if (length(id_var) == 1L)
    ID <-  x[,id_var]
  else {
    ID <- do.call("paste", c(x[,id_var], sep = "\r"))
    idDF <- cbind(x[,id_var], ID)
  }
  ## splitting per id (one line per list element)
  splitted <- split(x[, !(names(x) %in% id_var)], f = factor(ID))
  ## Missing variable per units
  nas <- lapply(splitted, function(x) names(x)[is.na(x)])
  ## Creating a similar data structure for id
  ids <- mapply(FUN = rep, names(nas), lapply(nas, length))
  ## putting all together
  res <- as.data.frame(do.call("rbind", mapply(cbind, ids, nas)))
  ## back to original id (not pasted one) for multiple id columns data.frame
  if (length(id_var) > 1L){
    res <- merge(idDF, res, all.x = TRUE, by.x = "ID", by.y = "V1")
    res$ID <- NULL
  }
  names(res) <- c(id_var, "variable")
  return(res)
}

#' remove NA and make a message with number of units deleted
#'
#' remove NA and make a message with number of units deleted
#' @param x something handled by \code{stats::na.omit}
#' @param verbose logical: if \code{TRUE} raise a message if any row is
#'     deleted
#'@export
NA_remove <- function(x, verbose = TRUE){
    y  <- stats::na.omit(x)
    nx <- nrow(x)
    ny <- nrow(y)
    if (verbose)
        if (nx != ny)
            message('Rows were' , nx, ', now are ', ny, '. ',
                    nx - ny, ' rows deleted due missingness.')
    return(y)
}
