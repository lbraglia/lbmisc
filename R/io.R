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

#' Create a .zip for SAS (data + sas file)
#'
#' Create a .zip for SAS (data + sas file)
#'
#' @param x a data.frame
#' @param file path to the zip file to save, if missing a .zip
#'     with the same name as the data.frame will be saved in the
#'     current working directory
#' @export
write_sas <- function(x = NULL, file = NULL){

    xname <- deparse(substitute(x))
    if (!is.data.frame(x)) stop('x must be a data.frame.')

    csv_file <- paste0(xname, '.csv')
    sas_file <- paste0(xname, '.sas')

    on.exit({ unlink(sas_file); unlink(csv_file) })

    if (is.null(file)) file <- paste0(xname, '.zip')

    foreign::write.foreign(df = x,
                           datafile = csv_file,
                           codefile = sas_file,
                           package = 'SAS')

    utils::zip(zipfile = file, files = c(csv_file, sas_file))
}
