standard_na_handler <- function(x) if (is.na(x)) "None" else x
char_na_handler <- function(x) if (is.na(x)) "None" else sprintf('"%s"', x)

out_int <- function(n, x){
  x_na <- sapply(x, standard_na_handler)
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}

out_num <- function(n, x){
  x_na <- sapply(x, standard_na_handler)
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}

## Original
## --------
## out_char <- function(n, x){
##   x_na <- sapply(x, char_na_handler)
##   sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
## }

# if a string contains quotes or a column name has special characters, the
# output breaks
out_char <- function(n, x){
  x_na <- sapply(x, function(val) {
    if (is.na(val)) "None" else sprintf('"%s"', gsub('"', '\\"', val))
  })
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}

out_logical <- function(n, x){
  x_na <- sapply(x, function(val) {
    if (is.na(val)) "None" else if (val) "True" else "False"
  })
  sprintf('"%s": [%s],\n', n, paste(x_na, collapse = ", "))
}

out_date <- function(n, x){
  x_na <- sapply(x, function(val) {
    if (is.na(val)) "None" else sprintf('"%s"', as.character(val))
  })
  sprintf('"%s": pl.Series([%s], dtype=pl.Date),\n', n, paste(x_na, collapse = ", "))
}

out_datetime <- function(n, x){
  x_na <- sapply(x, function(val) {
    if (is.na(val)) {
      "None"
    } else {
      # Format as ISO 8601 string; adjust precision as needed
      sprintf('"%s"', format(val, "%Y-%m-%dT%H:%M:%S"))
    }
  })
  sprintf('"%s": pl.Series([%s], dtype=pl.Datetime("us")),\n', n, paste(x_na, collapse = ", "))
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
      # Order matters: check Date before numeric (Date is a subclass of numeric)
      if (inherits(x, "Date")) {
        cat(out_date(n, x))
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        cat(out_datetime(n, x))
      } else if (is.logical(x)) {
        cat(out_logical(n, x))
      } else if (is.integer(x)) {
        cat(out_int(n, x))
      } else if (is.numeric(x)){
        cat(out_num(n, x))
      } else if (is.character(x)){
        cat(out_char(n, x))
      } else if (is.factor(x)){
        cat(out_factor(n, x))
      } else {
        # Unknown type; silently skip
      }
    }

    if (!is.data.frame(df)) stop("Input must be a data.frame")
    if (nrow(df) == 0) warning("Empty dataframe")
    if (any(duplicated(names(df)))) stop("Duplicate column names")

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


# Create a test dataframe with all supported types
test_df <- data.frame(
  # Integer
  id = c(1L, 2L, 3L, 4L, NA),
  # Numeric (double)
  score = c(9.5, 8.2, NA, 7.1, pi),
  # Character
  name = c("Alice", NA, "Charlie", "Diana", "Eve"),
  # Logical
  active = c(TRUE, FALSE, NA, TRUE, FALSE),
  # Date
  birth_date = as.Date(c("1990-01-15", "1985-06-20", "1992-03-10", NA, "1988-11-05")),
  # DateTime (POSIXct)
  created_at = as.POSIXct(
    c("2023-01-15 14:30:00", 
      "2023-06-20 08:15:00", 
      "2023-03-10 16:45:30", 
      "2023-08-05 12:00:00", 
      NA),
    tz = "UTC"
  ),
  # Factor with NA
  category = factor(
    c("A", "B", "A", NA, "C"),
    levels = c("A", "B", "C")
  )
)

pldf(test_df, "/tmp/test.py")
