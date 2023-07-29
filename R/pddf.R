out_int <- function(n, x)
    sprintf('"%s": [%s],\n', n, paste(x, collapse = ", "))
out_num <- function(n, x)
    sprintf('"%s": [%s],\n', n, paste(format(x, digits = 10), collapse = ", "))
out_char <- function(n, x)
    sprintf('"%s": [%s],\n', n, paste(sprintf('"%s"', x), collapse = ", "))
out_factor <- function(n, x){}

#' Generate an ascii representation of a dataframe suitable for pandas
#' 
#' It's a dput for python pandas dataframe
#' 
#' @param df a data.frame
#' @param outfile a .py file path or the dataframe will be printed on
#'     stdout
#' @param dfname name given to the pd.DataFrame
#' @examples
#' pddf(Indometh)
#' pddf(data.frame("a" = pi))
#' 
#' @export
pddf <- function(df, outfile = '', dfname="df"){
    dispatch <- function(x, n){
        if (is.integer(x)) {
            cat(out_int(n, x))
        } else if (is.numeric(x)){
            cat(out_num(n, x))
        } else if (is.character(x)){
            cat(out_char(n, x))
        } else if (is.factor(x)){
            cat(out_factor(n,x))
        } else {
            # pass
        }
    }

    header <- sprintf("\n%s = pd.DataFrame({\n", dfname)
    footer <- "})\n"
    if (outfile != "") sink(outfile)
    cat(header)
    Map(dispatch, as.list(df), as.list(names(df)))
    cat(footer)
    if (outfile != "") sink()
}
