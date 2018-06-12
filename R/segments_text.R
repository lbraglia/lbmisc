#' Display a line with text above in a figure
#' 
#' especially useful for boxplot (p-value comparison of groups)
#'
#' @param text text displayed
#' @param pos text position regarding the line
#' @param x0 line coordinates x_0 (by default = 1, to ease boxplots)
#' @param x1 line coordinates x_1 (by default = 2, to ease boxplots)
#' @param y line coordinates y (horizontal line only)
#' @param lty line lty
#' 
#' @export
segments_text <- function(text,
                          pos = c('above', 'below'),
                          x0 = 1, x1 = 2, y, lty = 'dotted'){
    pos <- match.arg(pos)
    pos <- switch(pos, below = 1, above = 3)
    graphics::segments(x0 = x0, x1 = x1, y0 = y, y1 = y, lty = lty)
    graphics::text(x = x0 + (x1 - x0)/2, y = y,
                   pos = pos, 
                   labels = text)
}
