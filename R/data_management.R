#' Handle other specify questions in a sensible way
#' 
#' @param x a character vector
#' 
#' @export
altro_specificare <- function(x) {
    tmp <- rm_spaces(tolower(x))
    tmp[tmp %in% ""] <- NA
    levs <- names(sort(table(tmp), decreasing = TRUE))
    ## order descendingly according to total counts
    factor(tmp, levels = levs)
}


#' Print the names of x as list for easy paste/copy in excel
#'
#' @param x the object with names
#' 
#' @export
names_list <- function(x) 
    cat(paste(names(x), collapse = '\n'), '\n', sep = "")

#' Print the unique values of the elements composing x 
#'
#' @param x the object (tipically data.frame or list)
#' 
#' @export
print_unique_values <- function(x) {
    print(lapply(x, function(y) sort(unique(y), na.last = TRUE)))
    invisible(NULL)
}


#' Make a factor from character removing all blanks "" values (setting to NA)
#'
#' @param x character
#' 
#' @export
factor_blankNA <- function(x){
    x <- rm_spaces(x)
    x[x %in% ""] <- NA
    factor(x)
}

#' Import an italian or english no/si-yes character variable and make
#' a factor
#' 
#' @param x character (or 0-1 numeric) the variable
#' @param labels labels to be applied to the factor
#' 
#' @export
nosi_import <- function(x, labels = c("No", "Yes")){
    if (is.numeric(x)){
        if (! all(x %in% c(0,1,NA))) stop("x must be a 0-1 variable") 
        factor(x, levels = c(0, 1), labels = labels)
    } else if (is.character(x) {
        x <- tolower(x)
        x <- rm_spaces(x)
        ## keep only the first lowerized letter that should be n, y or s
        first <- substr(x, 1, 1)
        ## make it all english
        first <- gsub("s", "y", x)
        first[first %in% ''] <- NA
        factor(first, levels = c('n', 'y'), labels = labels)
    } else stop("x must be numeric 0-1 or character")
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


#' Determine all duplicate elements
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
    if (all) {duplicated(x, ...)| duplicated(x, fromLast = TRUE, ...)}
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
#' @return a data.frame with non-passed checks
#' @examples
#'
#' (data <- data.frame(id = letters[1:4],  x = c(1,2,3,4), y = c(0,1,3,NA), z = rep(0, 4)))
#' compare_columns(data[, -1], operator= '<')
#' compare_columns(data[, -1], operator= '>')
#' compare_columns(data[, -1], operator= '<', row_id = data$id)
#' 
#' @export
compare_columns <- function(db,
                            operator = "<",
                            row_id = NULL)
{

    ## data should be a data.frame with no characters
    stopifnot(is.data.frame(db))

    if (any(unlist(lapply(db, is.character))))
        warning('There are characters in db.')

    db_names <- names(db)
    flag1 <- "AT MOST ONE NON-MISSING VALUE"
    flag2 <- "ROW ORDER IS OK"
    
    test_row <- function(x){
        not_NA_vars <- !is.na(x)
        if (sum(not_NA_vars) > 1L){
            # select only not NA values 
            not_NA_values <- x[not_NA_vars]
            names(not_NA_values) <- db_names[not_NA_vars]
            ## operands
            first <- not_NA_values[- length(not_NA_values)]
            second <- not_NA_values[- 1]
            comparison <- Reduce(operator, list(first, second ))
            names(comparison) <- sprintf("'%s' vs '%s'", names(first), names(second))
            wrong <- names(comparison)[comparison %in% FALSE]
            if (length(wrong) >= 1) wrong else flag2
        } else flag1
    }

    ## list of checks
    check_list <- as.list(apply(db, 1, test_row)) # as.list to be confident of having a list
    names(check_list) <- if(!is.null(row_id)) row_id else as.character(seq_along(check_list))
    useful <- unlist(lapply(check_list, function(x) x[1] %nin% c(flag1, flag2)))
    check_list <- check_list[useful]

    if (length(check_list) > 0){
        ## return data.frame of checks
        check_df <- Map(function(data, rows_id) data.frame('record' = rows_id, 'issue' = data),
                        check_list, names(check_list))
        check_df <- do.call(rbind, check_df)
        rownames(check_df) <- NULL
        names(check_df)[1] <- if (!is.null(row_id)) 'id' else 'record'
        check_df
    } else {
        # return nothing
        invisible(NULL)
    }
}


#' Vector recode utility
#' 
#' 
#' Yet another recode utility
#' 
#' @param x Vector to be recoded.
#' @param from_to matrix containing 2 columns (first is "from", second
#'     is "to") or a vector with even number of components where odds
#'     index are taken as from, even are taken
#' @return A vector with new codes (if/where modified).
#' @examples
#' test <- c(1:10, NA)
#' recode.m <- matrix(c(1, 2, NA, 3, 4, 1), nrow = 3, ncol = 2)
#' cbind(test, recode(test, recode.m))
#' 
#' test2 <- c(letters[1:10], NA)
#' recode.m2 <- matrix(c(c(letters[1:3]),c(NA,LETTERS[2:3])),
#'                     nrow = 3, ncol = 2) 
#' cbind(test2, recode(test2, recode.m2))
#'
#' \dontrun{
#' recode.m3 <- matrix(c(1, 2, 4,4,5:8),ncol=2)
#' test3 <- c(1:10,NA)
#' cbind(test3, recode(test3, recode.m3))
#' }
#' @export
recode <- function(x = NULL, from_to = NULL)
{
    ## A few data input checks
    if(! (mode(x) %in% c("numeric", "character")))
        stop("x must be a numeric or character vector.")
    
    from_to <- validate_recode_directives(from_to)

    ## Apply directive to vector
    unlist(lapply(x, function(y) {
        recode_row <- from_to[, 1] %in% y
        if (TRUE %in% recode_row) from_to[recode_row, 2]
        else y
    }))
}

#' Comment several variables of a data.frame
#'
#' Comment several variables of a data.frame using a for loop of \code{comment}
#' @param x a data.frame
#' @param var_com variable name/comment pair
#' @param quiet warning if some variable are not found in x (FALSE by default)
#' @examples
#' db <- data.frame(a = letters, b = LETTERS)
#' db <- comment_df(x = db, c('a', 'lowercase letters',
#'                            'b', 'uppercase letters'))
#' @export
comment_df <- function(x, var_com, quiet = FALSE){
    stopifnot(is.data.frame(x),
              is.character(var_com))

    var_com <- validate_recode_directives(var_com)
    var_names <- var_com[,1]
    var_comments <- var_com[,2]
    xnames <- names(x)
    
    ## handle missing variable names
    missing_names <- var_names[var_names %nin% xnames]
    if (length(missing_names) > 0L && (!quiet)){
        missing_names <- paste(missing_names, collapse = ', ')
        msg <- 'There are variables not found in x (%s); ignoring them ...'
        msg <- sprintf(msg, missing_names)
        warning(msg)
    }

    ## x names that have no comment in var_com
    missing_names2 <- xnames[xnames %nin% var_names]
    if (length(missing_names2) > 0L && (!quiet)){
        missing_names2 <- paste(missing_names2, collapse = ', ')
        msg <- 'Some x variables have no comment in var_com (%s); bypassing ..'
        msg <- sprintf(msg, missing_names2)
        warning(msg)
    }


    
    for (i in var_names){
        if (i %in% xnames){
            vc <- var_comments[var_names %in% i]
            comment(x[, i]) <- vc
        }
    }
    x
}


## helper function to uniform and validate from_to (a recode
## parameter) and var_com (comment_df parameter)
validate_recode_directives <- function(x){

    stopifnot(is.atomic(x),
              !is.null(x))

    if (is.null(dim(x))){
        ## x è un vettore
        if (length(x) %% 2 != 0)
            stop('x must be a even length vector')
        x <- matrix(x, ncol = 2, byrow = TRUE)
    } else if(is.matrix(x)) {
        if (ncol(x) != 2)
            stop("x must be a matrix with 2 columns.")
    } else
        stop("x must be a vector or a matrix")

    ## Checking for recoding directives uniqueness
    x <- unique(x)
    if(anyDuplicated(x[, 1]))
        stop("No univocal recoding directives")
    x
}
