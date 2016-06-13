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
