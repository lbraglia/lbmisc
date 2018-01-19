#' make a data.frame from a table of counts
#'
#' make a data.frame from a table of counts
#' 
#' @param x a frequency table (without margins)
#' 
#' @export
table2df <- function(x){
    df1 <- as.data.frame(x)
    indexes <- rep(seq_len(nrow(df1)), df1$Freq)
    rval <- df1[indexes, names(df1) %without% 'Freq']
    rownames(rval) <- NULL
    rval
}
