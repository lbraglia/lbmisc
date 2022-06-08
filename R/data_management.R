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
#' @param lowcase apply tolower?
#' 
#' @export
factor_blankNA <- function(x, lowcase = TRUE){
    x <- rm_spaces(x)
    if (lowcase) x <- tolower(x)
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
    } else if (is.character(x)) {
        x <- tolower(x)
        x <- rm_spaces(x)
        ## keep only the first lowerized letter that should be n, y or s
        first <- substr(x, 1, 1)
        ## make it all english
        first <- gsub("s", "y", first)
        ## handle characters "0" and "1"
        first <- gsub("1", "y", first)
        first <- gsub("0", "n", first)
        ## NA to blanks
        first[first %in% ''] <- NA
        factor(first, levels = c('n', 'y'), labels = labels)
    } else stop("x must be numeric 0-1 or character")
}

#' Import an italian or english male/female character variable and
#' make a factor
#' 
#' @param x character the variable containing gender information (M/F)
#' @param labels labels to be applied to the factor
#' 
#' @export
gender_import <- function(x, labels = c("M", "F")){
    if (is.character(x)) {
        x <- tolower(x)
        x <- rm_spaces(x)
        ## keep only the first lowerized letter that should be m or f
        first <- substr(x, 1, 1)
        factor(first, levels = c('m', 'f'), labels = labels)
    } else stop("x must be character")
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
    as.integer(res)
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
#' Compare columns progressively in a dataset using a specified
#' operator, that tells how columns should be ordered (eg by default
#' columns should be increasing)
#' @param db a data.frame with ordered columns
#' @param operator comparison operator
#' @param row_id an optional row id
#' @param extended logical if TRUE
#' @return a list of checks with values or (if extended = FALSE) a
#'     compact data.frame with non-passed checks and variable mentioning
#' @examples
#'
#' (data <- data.frame(id = letters[1:4],  x = c(1,2,3,4), y = c(0,1,3,NA), z = rep(0, 4)))
#' compare_columns(data[, -1], operator= '<')
#' compare_columns(data[, -1], operator= '>')
#' compare_columns(data[, -1], operator= '<', row_id = data$id)
#' compare_columns(data[, -1], operator= '<', row_id = data$id, extended = FALSE)
#' 
#' @export
compare_columns <- function(db = NULL,
                            operator = "<",
                            row_id = seq_len(nrow(db)),
                            extended = TRUE)
{
    ## data should be a data.frame with no characters
    if (!is.data.frame(db)) stop("db must be a data.frame")
    if (anyDuplicated(row_id)) stop("row_id can't contains duplicated")
    if (length(row_id) != nrow(db)) stop("row_id must be have the same length of dim(db)")
    if (any(unlist(lapply(db, is.character)))) warning('There are characters in db.')
    ## for referencing in test_row
    db_names <- names(db)
    ## main worker, applied to each row
    test_row <- function(x, id){
        not_NA_vars <- !is.na(x)
        if (sum(not_NA_vars) > 1L){
            # select only not NA values 
            not_NA_values <- x[not_NA_vars]
            names(not_NA_values) <- db_names[not_NA_vars]
            ## operands
            first <- not_NA_values[- length(not_NA_values)]
            second <- not_NA_values[- 1]
            nf <- names(first)
            ns <- names(second)
            ok <- Reduce(operator, list(first, second))
            res <- data.frame('first' = nf, 'second' = ns)
        } else {
            ok <- TRUE
            res <- data.frame('first' = NA, 'second' = NA)
        }
        res[, 'compare_columns_id'] <- id
        ## output only non passed checks
        res[ok %in% FALSE, c('compare_columns_id', 'first', 'second'), drop = FALSE]
    }
    ## apply the row_checker to all rows
    db_spl <- split(db, row_id)
    checks_list <- Map(test_row, db_spl, as.list(row_id))
    checks <- na.omit(do.call(rbind, checks_list))
    rownames(checks) <- NULL
    ## extended list
    checks_spl <- split(checks, f = with(checks, list(first, second)), sep = ' vs ')
    retriever <- function(check){
        ## variabili tenute del merge
        uvars <- unique(check[, c('first', 'second')])
        selected_vars <- c('compare_columns_id', unlist(uvars))
        res <- merge(x = check, y = cbind(data.frame('compare_columns_id' = row_id), db),
                     by = 'compare_columns_id', all.x = TRUE)[, selected_vars]
        ## return ordered data.frame (if the merge is ok) or null if there are
        ## no problems
        if (is.data.frame(res)) {
            names(res)[1] <- 'id'
            res[order(res$id), ]
        }
        else NULL
    }
    extended_list <- lapply(checks_spl, retriever)
    extended_list <- Filter(function(x) !is.null(x), extended_list)
    ## return
    if (nrow(checks) > 0) {
        if (extended) extended_list else checks
    } else {
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
    if(anyDuplicated(x[, 1])){
        dups <- unique(x[, 1][duplicated(x[, 1])])
        stop("No univocal recoding directives for: ",
             paste(dups, collapse = ", "),
             ".")
    }
    x
}

#' test equality of two vectors without leaving NA
#'
#' test equality of two vectors without leaving NA as output (aka
#' considering NA = NA are equal, so TRUE, and value = NA are diverse,
#' so FALSE)
#' 
#' @param x first vector
#' @param y second vector
#' @param one_NA what to output if only one of the two provided is NA
#'     (default = TRUE, aka value are diverse)
#' @param both_NA what to output if only one of the two provided is NA
#'     (default = TRUE, aka value are diverse)
#' @examples
#' a <- c(NA, NA, 1, 2)
#' b <- c(1,  NA, 2, 2)
#' equal(a, b)
#' 
#' @export
equal <- function(x, y, one_NA = FALSE, both_NA = TRUE) {
    ## base comparison
    res <- x == y
    ## if one NA and the other not NA set equal to FALSE
    only_one_NA <- xor(is.na(x), is.na(y))
    res[only_one_NA] <- one_NA
    ## if both are NA set equal to TRUE
    both_are_NA <- is.na(x) & is.na(y)
    res[both_are_NA] <- both_NA
    res
}


#' coerce a character numeric with , to numeric
#'
#' @param x character numeric with , to numeric
#' 
#' @export
as.numeric2 <- function(x){
    as.numeric(gsub(",", ".", x))
}

#' convert a logical to a factor
#'
#' @param x logical vector (FALSE -> NO, TRUE -> Yes)
#' @export
ft2ny <- function(x) factor(x,
                            levels = c(FALSE, TRUE),
                            labels = c("No", 'Yes'))


#' ease recist codes import
#'
#' @param x character recist code such as CR, CP etc
#' 
#' @export
recist_import <- function(x){
    ## uniform italian to english
    ft <- c("RC", "CR", 
            "RP", "PR")
    ## and make a factor
    factor(lbmisc::recode(x, from_to = ft),
           c("CR", "PR", "SD", "PD"))
}
