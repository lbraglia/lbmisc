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
