#' Add grid to a plot
#'
#' 
#' Add grid to a plot using \code{\link{abline}}.
#'
#' 
#' @param at.x x axis grid values
#' @param at.y y axis grid values
#' @param col grid color
#' @param lty line type
#' @param lwd line width
#' @return The function adds a grid to a plot using \code{\link{abline}}.
#' @export
add_grid <- function(at.x, at.y = at.x,
                     col = "lightgray", 
                     lty = "dotted", 
                     lwd = par("lwd")) {
  abline(v = at.x, col = col, lty = lty, lwd = lwd)
  abline(h = at.y, col = col, lty = lty, lwd = lwd)
}

#' Add cartesian system to a plot
#'
#' 
#' Add a cartesian system to a plot using \code{\link{abline}}.
#'
#' 
#' @param at.x x centre of the system
#' @param at.y y centre of the system
#' @param col grid color
#' @param lty line type
#' @param lwd line width
#' @return The function adds a cartesian plane to a plot using
#' \code{\link{abline}}. 
#' @export
add_cartesian_plane <- function(at.y = 0, at.x = 0,
                                col = "black", 
                                lty = "solid", 
                                lwd = par("lwd")) {
    abline(v = at.x, col = col, lty = lty, lwd = lwd)
    abline(h = at.y, col = col, lty = lty, lwd = lwd)
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
        m <- ceiling(sqrt(n <- length(cl <- colors())))
        length(cl) <- m*m; cm <- matrix(cl, m)
        op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
        plot(1:m,1:m, type="n", axes=FALSE)
        text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
    }

    showCols.grid <- function(bg=NULL, cex = NULL, rot = NULL) {
        m <- ceiling(sqrt(n <- length(cl <- colors())))
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
    plot(rx, ry, type = "n", axes  =  FALSE, 
         xlab = "", ylab = "", 
         main = "Pch symbols", 
         sub=sprintf("points (...,  pch = *, cex = '%s', col='%s', bg= '%s')",
             cex, col, bg) ) 
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
        pc <- pch[[i]]
        ## 'col' symbols with a 'bg'-colored interior (where available) :
        points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
        if(cextext > 0)
            text(ix[i] - 0.3, iy[i], pc, col = "black", cex = cextext)
    }
    
    ## ShowPch()
    ## ShowPch(c("o","O","0"), cex = 2.5)
    ## ShowPch(NULL, cex = 4, cextext = 0)

}
