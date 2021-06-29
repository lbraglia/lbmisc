#'
#' Alternative to View that uses openXL
#'
#' Alternative to \code{\link{View}} that invokes a spreasheet viewer on
#' openxlsx's \code{\link{writeData}} handled objects. 
#'
#' @param ... writeData handled objects
#' @param freezePane apply freezePane at 'A2', for each sheet?
#' @param autoColWidth apply auto column widths to all sheets?
#' @examples \dontrun{
#' view(Indometh, iris)
#' }
#' @export
view <- function(... , freezePane = TRUE, autoColWidth = TRUE )  {
  wb <- openxlsx::createWorkbook()
  objList <- list(...)
  objNames <- abbreviate(as.character(match.call(expand.dots = TRUE))[-1], 31)
  mapply(openxlsx::addWorksheet,
         sheetName = objNames,
         MoreArgs = list(wb = wb))
  mapply(openxlsx::writeData,
         sheet = objNames,
         x = objList,
         MoreArgs = list(wb = wb)) 
  ## FreezePane
  if (freezePane) {
    mapply(openxlsx::freezePane,
           sheet = objNames,
           MoreArgs = list(wb = wb, firstRow = TRUE))
  }
  ## autoColWidth
  if (autoColWidth) {
    ncols <- lapply(lapply(objList, ncol), seq)
    mapply(openxlsx::setColWidths,
           sheet = objNames,
           cols = ncols, 
           MoreArgs = list(wb = wb, widths = "auto"))
  }
  openxlsx::openXL(wb)
}
