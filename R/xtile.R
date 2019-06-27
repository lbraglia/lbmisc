#' Cut numeric x in quantile
#' 
#' 
#' Cut numeric x in quantile
#' 
#' 
#' @param x numeric vector
#' @param n number of quantiles
#' @return A factor vector.
#' @examples
#' 
#' tertiles <- xtile(rnorm(1000),3)
#' table(tertiles)
#' 
#' @export
xtile <- function(x, n) {
  cuts <- quantile(x, probs = seq(0, 1, length = n + 1), na.rm = TRUE)
  cut(x, breaks = cuts, include.lowest=TRUE)
}
