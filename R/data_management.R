#' check if a object is unique
#'
#' check if a object is unique; this is a simple wrapper useful for code
#' readability
#' @param x a object that can be passed to \code{\link{duplicated}}
#' @rdname is_unique
#' @export
is.unique <- function(x = NULL) !(any(duplicated(x)))

#' Determine all duplicate elements
#' 
#' 
#' base::duplicated determines which elements of a vector or data frame are
#' duplicates of elements with smaller subscripts, and returns a logical vector
#' indicating which elements (rows) are duplicates.
#' 
#' This wrapper allows to find all duplicated obs.
#' 
#' @param x Numeric vector of centimeter.
#' @param all Logical: if TRUE considers duplicated each observation
#' present more than one time in the object, otherwise implement
#' base::duplicated algorithm (considers duplicated the second, third and so on
#' duplicated observation).
#' @param ... Other arguments passed to base::duplicated.
#' @return A logical vector with duplicated marked as TRUE.
#' @examples
#' duplicated( c(1,0, 2, 0, 3, 2)) # from base R
#' duplicated2(c(1,0, 2, 0, 3, 2)) # from lbmisc
#' @export
duplicated2 <- function(x, all = TRUE, ...)
{
    if (all) {duplicated(x, ...)| duplicated(x, fromLast = TRUE)}
    else {duplicated(x, ...)}
}

#' Compare columns progressively
#'
#' Compare columns progressively in a dataset using a specified operator,
#' that tells how columns should be ordered (eg by default columns should
#' be increasing)
#' @param db a data.frame with ordered columns
#' @param operator comparison operator
#' @param row_id an optional row id
#' @param short logical, whether to include only a report (TRUE) or a matrix
#' with all comparisons results (FALSE) 
#' @return a list with raw results and a report
#' @examples
#' 
#' compare_columns(data.frame(a = c(1,2,3), b = c(0,1,4)),
#'                 row_id = letters[1:3])
#' 
#' @export
compare_columns <- function(db,
                            operator = "<",
                            row_id = NULL,
                            short = TRUE)
{

  ## data should be a data.frame with no characters
  stopifnot( (is.data.frame(db)) & (!any(sapply(db, is.character))) )
  first <- db[, -ncol(db), drop = FALSE]
  second <- db[, -1, drop = FALSE]

  ## matrix results
  res <- Reduce(operator, list(first, second ))
  colnames(res) <- paste(names(first), names(second), sep = ".vs.")
  if(!is.null(row_id))
    rownames(res) <- row_id

  ## A report with all test that doesn't respect the rule, but selecting
  ## only useful records for print (those with at least 1 query/flag)
  report <- apply(res, 1, function(x) names(x)[x %in% FALSE])
  select <- unlist(lapply(report, function(x) length(x)>0))

  if (short)
    return(report[select])
  else
    return(list("results" = res, "report" = return(report[select])))
}

#' Vector recode utility
#' 
#' 
#' Yet another recode utility
#' 
#' 
#' @param x Vector to be recoded.
#' @param rec matrix containing 2 columns; first is "from", second is
#' "to".
#' @return A vector with new codes (if/where modified).
#' @export
recode <- function(x = NULL, rec = NULL)
{
    ## A few data input checks
    if( !is.vector(x) ) 
        stop("A vector to be recoded must be given.")
    if( !is.matrix(rec)| ncol(rec)!=2 ) 
        stop("I need a 2 column matrix with recoding directives: ",
             "first column is 'from', second 'to'.")
	
    ## Checking for recoding directives uniqueness
    rec <- unique(rec)
    if( any(duplicated(rec[,1])) )
        stop("No univocal recoding directives")

    to.be.recoded <- x %in% rec[,1]
    
    search.n.replace <- function(search.me, where, replace.me) {
        my.res <- rep(NA, length(where))
        my.res[ where  %in%  search.me] <- replace.me
        my.res
    }
	
    partial.results <- unname(apply(rec, 1,
                                    function(x) search.n.replace(
                                        search.me=x[1],
                                        where=x,
                                        replace.me=x[2]) )) 
	
    recoded.and.NA <- unlist(apply( partial.results, 1,
                                   function(x) ifelse(
                                       any(!is.na(x)) ,
                                       x[!is.na(x)], NA ) ) ) 
	
    ## A value could be NA due to a recode; because of this
    ## ...(is.na(recoded.and.NA) & to.be.recoded) 
    ifelse( !is.na(recoded.and.NA) | (is.na(recoded.and.NA) & to.be.recoded),
           recoded.and.NA, 
           x)

    ## Example
    ## test <- c(1:10,NA)
    ## recode.m <- matrix(c(1,2,NA,3,4,1), nrow=3,ncol=2)
    ## cbind(test, recode( test, recode.m))
    
    ## test2 <- c(letters[1:10],NA)
    ## recode.m2 <- matrix(c(c(letters[1:3]),c(NA,LETTERS[2:3])),
    ## nrow=3,ncol=2) 
    ## cbind(test2, recode(test2, recode.m2))

    ## recode.m3 <- matrix(c(1,2,4,4,5:8),ncol=2)
    ## test3 <- c(1:10,NA)
    ## cbind(test3, recode(test3, recode.m3))

}

#' Group progressive id creator
#'
#' Starting from a vector of group id, this function creates a progressive
#' id inside each group.  It uses C for efficiency
#' 
#' @param group a group vector
#' @examples
#' set.seed(1)
#'
#' ## One factor id
#' x <- sample(c(rep("A",5), rep("C",3), rep("B",2), rep(NA,2)))
#' data.frame(group = x, id = group_prog_id(x) )
#'
#' ## Two factors id
#' y <- sample(gl(2,6, labels = c("C","D")))
#' data.frame(group1 = x, group2 = y, id = group_prog_id(interaction(x,y)) )
#'
#' ## example of sorting
#' db <- data.frame("group" = sample(gl(2,5)), "b" = Sys.Date() + 1:10)
#' db <- db[order( - as.integer(db$group), db$b),]
#' data.frame(db, id = group_prog_id(db$group))
#' @export
group_prog_id <- function(group) {

  ## make NA as a valid level for low-level operations
  group2 <- as.integer(factor(group, exclude = NULL))
  res <- .Call("group_prog_id_slave",
               group2,
               max(group2, na.rm=TRUE),
               package = "lbmisc")
  ## handling NA
  res[is.na(group)] <- NA
  res
}
