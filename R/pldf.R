standard_na_handler <- function(x) if (is.na(x)) "None" else x
char_na_handler <- function(x) if (is.na(x)) "None" else sprintf('"%s"', x)

out_int <- function(n, x){
  x_na <- sapply(x, standard_na_handler)
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}
out_num <- function(n, x){
  x_na <- sapply(x, standard_na_handler)
  sprintf('"%s": [%s],\n', n, paste(format(x_na, digits = 10), collapse = ", "))
}
out_char <- function(n, x){
  x_na <- sapply(x, char_na_handler)
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}
out_factor <- function(n, x){
  x_na <- sapply(as.character(x), char_na_handler)
  labels <- levels(x)
  enum <- sprintf(
    '"%s": pl.Series([%s], dtype=pl.Enum([%s])),\n',
    n,
    paste(x_na, collapse = ", "),
    paste(sprintf('"%s"', labels), collapse = ", ")
  )
  enum
}

#' Generate an ascii representation of a dataframe suitable for polars
#'
#' It's a dput for python polars dataframe (use lb.io.import_data to import them)
#'
#' @param df a data.frame
#' @param outfile a .py file path or the dataframe will be printed on stdout
#' @examples
#'
#' pldf(Indometh)
#' pldf(data.frame("a" = pi))
#'
#' @export
pldf <- function(df, outfile = ""){
    dispatch <- function(x, n){
      if (is.integer(x)) {
          cat(out_int(n, x))
        } else if (is.numeric(x)){
          cat(out_num(n, x))
        } else if (is.character(x)){
          cat(out_char(n, x))
        } else if (is.factor(x)){
          cat(out_factor(n, x))
        } else {
          # pass
        }
    }

    if (outfile != "") {
      path_components <- strsplit(tools::file_path_sans_ext(outfile), "/")[[1]]
      dfname <- path_components[length(path_components)]
    } else {
      dfname <- "df"
    }
    
    import <- "import polars as pl\n"
    header <- sprintf("%s = pl.DataFrame({\n", dfname)
    footer <- "})\n"
    if (outfile != "") sink(outfile)
    cat(import)
    cat(header)
    Map(dispatch, as.list(df), as.list(names(df)))
    cat(footer)
    if (outfile != "") sink()
}


## example/test
df <- Indometh
df$group <- c("a", "b", NA)
df$group <- factor(df$group, levels = c("a", "b"))
pldf(df)
df[2, ] <- NA
pldf(df)
pldf(df, "/tmp/indometh.py")

