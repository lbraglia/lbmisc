#' preprocess data
#' 
#' preprocess data according to variable name - function couple directives
#' 
#' @param x the data.frame
#' @param preprocessors a named list: names = function names to be applied, 
#'  content is variable to be treated with the function
#' @param mr_name name of preprocessors associated used for multiple response
#' @param mr_fun function used for multiple response which creates a data.frame
#'   of dummies given a single vector of multiple responses (eg 
#'   separated by "|")
#' @param ... argument passed to mr_fun
#' @examples
#' data_char <- function(d) as.Date(d, format = '%d/%m/%Y')
#' noyes <- function(x) factor(x, levels = c('NO', 'SI'),
#'                             labels = c('No', 'Yes'))
#' test_df <- data.frame('a_date'    = c('2017-01-01', '2015-01-01'),
#'                       'gender'    = c('m', 'f'),
#'                       'female'    = c('NO', 'SI'),
#'                       'educ_lev'  = c('diploma', 'degree'),
#'                       'birth'     = c("11/08/1927", "24/05/1935"),
#'                       'interests' = c('reading|travel|science',
#'                                       'reading|science|cinema|tv'),
#'                       stringsAsFactors = FALSE)
#' preprocessors <- list("as.Date" = 'a_date',
#'               "factor" = c("gender", "educ_lev"),
#'               "data_char" = 'birth',
#'               "noyes" = 'female',
#'               "mr" = "interests")
#' str(test_df)
#' preproc <- preprocess_data(test_df, preprocessors)
#' str(preproc)
#' @export
preprocess_data <- function(x, preprocessors, 
                            mr_name = 'mr',
                            mr_fun = smarty_mr_splitter,
                            ...)
{
    ## -------------------
    ## validate preprocessors list
    ## -------------------
    
    ## remove variable names that are not in the dataset
    not_in_x <- unlist(lapply(preprocessors,
                              function(vn) vn[vn %nin% names(x)]))
    if (length(not_in_x) > 0) {
        nixvars <- paste(not_in_x, collapse = ', ')
        message("Ignoring the following entries in preprocessors, ",
                "since no variables with those name have been ",
                "found in x: ", nixvars)
    }
    ## keep the good ones only
    preprocessors  <- lapply(preprocessors, function(y) y[y %nin% not_in_x])
    ## keep only non-null or 0 length chars elements
    preprocessors <- Filter(function(x) length(x) > 0L, preprocessors)
    ## check for duplicated entries which can create some hassle
    if (anyDuplicated(tmp <- unlist(preprocessors))){
        dups <- unique <- tmp[duplicated(tmp)]
        stop('Some variables in preprocessors are duplicated, aborting: ',
             paste(dups, collapse = ', '))
    }
    
    ## if there are non null elements
    if (length(preprocessors) > 0){

        ## -------------------------------------------------------
        ## start handling all the variables but multiple responses
        ## -------------------------------------------------------

        all_but_mr <- preprocessors[names(preprocessors) %without% mr_name]
        ## get the proper functions
        fs <- lapply(names(all_but_mr), get)
        ## repeat the function for each element
        var_names <- do.call(c, all_but_mr)
        var_counts <- unlist(lapply(all_but_mr, length))
        fs_rep <- fs[rep(seq_along(fs), var_counts)] 
        sel <- x[, var_names, drop = FALSE]
        x[, var_names] <- Map(verbose_coerce, 
                              x = sel, 
                              coercer = fs_rep,
                              xname = names(sel))
        
        ## ------------------
        ## multiple responses
        ## ------------------
        
        if (mr_name %in% names(preprocessors)){
            mr_vars <- preprocessors[[mr_name]]
            mr_pos <- match(mr_vars, names(x))
            mrs <- lapply(mr_vars, function(y) mr_fun(x[, y], ...))
            ## fix varnames
            renamer <- function(df, prefix) {
                names(df) <- gsub("x\\[, y\\]", prefix, names(df))
                df
            }
            mrs <- Map(renamer, mrs, mr_vars)
            ## split the original dataframe by columns dictated by mr_pos
            ## then add to the list the mrs 
            ## then cbind everything
            split_df_by_col_pos <- function(x, pos){
                if (any(pos > ncol(x))) stop('index out of range')
                ## unique is needed to handle the case where
                ## the first or the last column is a mr
                start <- unique(c(0, pos) + 1)
                ## do not go outside if last is a mr
                start <- start[start <= ncol(x)]
                stop <- unique(c(pos, ncol(x)))
                if (length(start) != length(stop)) 
                    stop("start and stop should have the same length")
                Map(function(df, from, to){
                    df[, seq(from = from, to = to), drop = FALSE]
                }, list(x), start, stop)
            }
            x_spl <- split_df_by_col_pos(x, mr_pos)
            ## append the original and multiple response lists
            ## and put the elements in the proper order for a direct cbind
            orig_len <- length(x_spl)
            mr_len <- length(mrs)
            ## in mr_index aggiungo un NA strumentale (per potere sfruttare
            ## il trucco di togliere la dimensione alla matrice)
            ## se l'ultima colonna non è una risposta 
            ## multipla (come dovrebbe avvenire nella maggioranza dei casi)
            orig_index <- seq_len(orig_len)
            mr_index <- if (orig_len == mr_len){
                            orig_len + seq_len(mr_len)
                        } else if (orig_len == mr_len + 1){
                            c(orig_len + seq_len(mr_len), NA)
                        } else {
                            stop('unhandled situation')
                        }
            ## ora sfrutto il trucco di togliere la dimensione
            ## alla matrice per avere gli indici alternati e tolgo
            ## il NA strumentale per la robustezza dell'algoritmo
            index_order <- c(rbind(orig_index, mr_index))
            index_order <- index_order[!is.na(index_order)]
            ## finish him!
            full_list <- c(x_spl, mrs)[index_order]
            x <- do.call(cbind, full_list)
        }
    }
    x
}


#' make a list of suitable preprocessors for omonimous argument in
#' preprocess_data
#'
#' make a list of data preprocessors suitable for preprocessors
#' argument in preprocess_data given an xlsx file properly constructed
#'
#' @param f xlsx file path (one sheet per data.frame, all are
#'     imported); the name of the sheet should be the name of the
#'     data.frame the preprocessors refers to; the first column (of
#'     each sheet) is a variable name; the other colnames are function
#'     names to be used with each variable marked. For each row
#'     (variable) put a "x" (btw '!is.na') is used to mark the
#'     variable with the proper coercer
#'
#' @examples \dontrun{
#'    proprocs <- get_preprocessors('./data/preprocessors.xlsx')
#'    db_clean <- preprocess_data(db, preprocs$db)
#' }
#' @export
get_preprocessors <- function(f = NULL){
    preprocessors <- read.xlsx_alls(f = f)
    ## imposto variable as rowname
    preprocessors <- lapply(preprocessors, function(pr){
        rownames(pr) <- pr[, 1]
        ## chck uniqueness
        pr[, 1] <- NULL
        duplicates <- rowSums(!is.na(pr)) > 1L
        if (any(duplicates)){
            duplicated_directives <- paste(rownames(pr)[duplicates],
                                           collapse = ', ')
            msg <- sprintf('Duplicated directives for: %s',
                           duplicated_directives)
            stop(msg)
        }
        pr
    })
    preprocessors <- lapply(preprocessors, function(pr){
        rval <- Map(function(col, rn) rn[!is.na(col)],
                    pr, list(rownames(pr)))
        ## keep only preprocessors with at least 1 variable
        Filter(function(x) length(x) > 0L, rval)
    })
    preprocessors
}
