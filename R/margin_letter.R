#' Function to draw a letter in a figure angle, after having plotted the figure
#'
#' Useful for multi-graph figures (eg with mfrow)
#' 
#' @param l text
#' @param pos location of the letter
#' @param cex cex parameter
#' @param xdr distance from the angle (x coordinates) as percentage of x range
#' @param ydr distance from the angle (y coordinates) as percentage of x range
#' @param ... further parameters passed to
#'
#' @examples
#' plot(rnorm(100))
#' margin_letter("A", "topleft")
#' margin_letter("B", "topright", xdr = 1/50) ## decrease distance from the angle, otherwise it's printed out of screen
#' margin_letter("C", "bottomleft")
#' margin_letter("D", "bottomright", xdr = 1/50)
#' @export
margin_letter <- function(l = NULL,
                          pos = c('topleft', 'topright',
                                  'bottomleft','bottomright'),
                          cex = par('cex') * 2,
                          xdr = 1/10, 
                          ydr = 1/10,
                          ...) 
{
    ## https://stackoverflow.com/questions/42034786/
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    pos <- match.arg(pos)
    ## Gets the four corners of plot area (x1, x2, y1, y2)
    corners <- par("usr") 
    x1 <- corners[1]
    x2 <- corners[2]
    y1 <- corners[3]
    y2 <- corners[4]
    xrange <- x2 - x1
    yrange <- y2 - y1
    ## Draw outside plot area
    par("xpd" = TRUE)
    if (pos == 'topleft') {
        x <- x1 - xrange * xdr
        y <- y2 + yrange * ydr
    } else if (pos == 'topright') {
        x <- x2 + xrange * xdr
        y <- y2 + yrange * ydr
    } else if (pos == 'bottomleft') {
        x <- x1 - xrange * xdr
        y <- y1 - yrange * ydr
    } else if (pos == 'bottomright') {
        x <- x2 + xrange * xdr
        y <- y1 - yrange * ydr
    } else stop("pos must be one of topleft, topright, ",
                "bottomleft, or bottomright")
    graphics::text(x = x, y = y, labels = l, cex = cex, ...)
    invisible(NULL)
}

