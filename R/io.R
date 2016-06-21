#' A function to save an openxlsx workbook in xls format quickly
#'
#' A function to export an openxlsx workbook oin xls format quickly
#' @param wb openxlsx workbook
#' @param destfile xls destination file
#' @export
wb_to_xls <- function(wb, destfile = NULL){
    xlsx_file <- paste0(tools::file_path_sans_ext(destfile), '.xlsx')
    on.exit(unlink(xlsx_file))
    openxlsx::saveWorkbook(wb, file = xlsx_file, overwrite = TRUE)
    lbmisc::unoconv(xlsx_file, format = 'xls')
    invisible(NULL)
}


#' Faster export with openxlsx
#'
#' This is a wrapper around addWorksheet and writeData to allow
#' less typing. It export one object per sheet, the sheet will be
#' created accordingly to the name given
#'
#' @param wb a Workbook object
#' @param sheet name of the sheet to be created
#' @param x object to be exported
#' @param ... further arguments passed to writeData
#' @export
add_to_wb <- function(wb = NULL, sheet = NULL, x = NULL, ...) {
    openxlsx::addWorksheet(wb = wb, sheetName = sheet)
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = x,
                        ...)
}
