#' Return an hex color
#'
#' Return an hex color with optional alpha shading
#' 
#' @param col a color
#' @param alpha alpha shading in [0, 1] interval (0 = invisible
#'     , 1 = no shading at all)
#' @examples
#' col2hex('blue', alpha = 0.5)
#' @export
col2hex <- function(col, alpha = 1L){
    stopifnot(alpha >= 0L,
              alpha <= 1L)
    grDevices::rgb(t(grDevices::col2rgb(col)),
                   maxColorValue = 255,
                   alpha = alpha * 255)
}



#' Add grid to a plot
#' 
#' Add grid to a plot using \code{\link{abline}}.
#' @param at_x x axis grid values
#' @param at_y y axis grid values
#' @param col grid color
#' @param lty line type
#' @param lwd line width
#' @param ... further parameter passed to abline
#' @return The function adds a grid to a plot using \code{\link{abline}}.
#' @export
add_grid <- function(at_x = NULL,
                     at_y = at_x,
                     col  = 'lightgray',
                     lty  = 'dotted',
                     lwd  = 1,
                     ...)
{
    if (is.null(at_x)) stop('at_x is mandatory')
    if (is.null(at_y)) stop('at_y is mandatory')
    graphics::abline(v = at_x, col = col, lty = lty, lwd = lwd, ...)
    graphics::abline(h = at_y, col = col, lty = lty, lwd = lwd, ...)
}

#' Add cartesian system to a plot
#'
#' Add a cartesian system to a plot using \code{\link{abline}}.
#' @param at_x x centre of the system
#' @param at_y y centre of the system
#' @param col grid color
#' @param lty line type
#' @param lwd line width
#' @param ... further parameter passed to abline
#' @return The function adds a cartesian plane to a plot using
#' \code{\link{abline}}. 
#' @export
add_cartesian_plane <- function(at_y = 0,
                                at_x = 0,
                                col = 'black',
                                lty = 'solid',
                                lwd = 1,
                                ...)
{
    if (is.null(at_x)) stop('at_x is mandatory')
    if (is.null(at_y)) stop('at_y is mandatory')
    graphics::abline(v = at_x, col = col, lty = lty, lwd = lwd, ...)
    graphics::abline(h = at_y, col = col, lty = lty, lwd = lwd, ...)
}
#' 
#' Show R colors for graphics and grid package
#' 
#' 
#' Show R colors for graphics and grid package (simple wrapper around functions
#' from demo(colors).
#' 
#' 
#' @param package charachter. Which package's color to show
#' @param bg charachter. Background color
#' @param cex numeric \code{cex}
#' @param srt.rot degree inclination
#' @return Nothing. As a side effect the plot of colors.
#' @examples
#' 
#' show_col()
#' 
#' @export
show_col <- function(package=c("graphics","grid"), 
                     bg="white",
                     cex = 0.75, 
                     srt.rot=30
                     )
{
   
    ## require("grid")
    ## require("graphics")

    package <- match.arg(package)
    
    showCols.graphics <- function(bg = NULL, cex = NULL, srt = NULL){
        m <- ceiling(sqrt(n <- length(cl <- grDevices::colors())))
        length(cl) <- m*m; cm <- matrix(cl, m)
        op <- graphics::par(mar=rep(0,4), ann=FALSE, bg = bg)
        on.exit(graphics::par(op))
        graphics::plot(1:m,1:m, type="n", axes=FALSE)
        graphics::text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
    }

    showCols.grid <- function(bg=NULL, cex = NULL, rot = NULL) {
        m <- ceiling(sqrt(n <- length(cl <- grDevices::colors())))
        length(cl) <- m*m; cm <- matrix(cl, m)
        grid::grid.newpage()
        vp <- grid::viewport(width = .92, height = .92)
        grid::grid.rect(gp=grid::gpar(fill=bg))
        grid::grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
                  vp=vp, gp=grid::gpar(cex = cex, col = cm))
    }

    if (package =="graphics") {
        showCols.graphics(bg = bg, cex = cex, srt = srt.rot)
    } else if (package =="grid") {
        showCols.grid(bg = bg, cex = cex, rot = srt.rot)
    }
    
    ## win.graph()
    ## ShowCols("graphics",bg="gray")
    ## win.graph()
    ## ShowCols("grid",bg="black")

}
#' 
#' Show \code{pch}
#' 
#' 
#' Show \code{pch} values.
#' 
#' 
#' @param extras charachter. Further symbols to be plotted
#' @param cex numeric \code{cex}
#' @param col color
#' @param bg character. Background color
#' @param cextext \code{cex} for text
#' @return Nothing. As a side effect the plot of \code{pch}.
#' @examples
#' 
#' show_pch()
#' 
#' @export
show_pch <-  function(extras = c("*",".","0","+","#"),
                      cex = 3, ## good for both .Device=="postscript" and "x11"
                      col = "red3", bg = "gold", cextext = 1.2 
                      ) {
        
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) ## list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    graphics::plot(rx, ry, type = "n", axes  =  FALSE, 
         xlab = "", ylab = "", 
         main = "Pch symbols", 
         sub=sprintf("points (...,  pch = *, cex = '%s', col='%s', bg= '%s')",
             cex, col, bg) ) 
    graphics::abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
        pc <- pch[[i]]
        ## 'col' symbols with a 'bg'-colored interior (where available) :
        graphics::points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
        if(cextext > 0)
            graphics::text(ix[i] - 0.3, iy[i], pc, col = "black",
                           cex = cextext)
    }
}
