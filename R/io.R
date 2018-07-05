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
#'
#' @examples
#' wb = openxlsx::createWorkbook()
#' add_to_wb(wb = wb, sheet = 'Indometh', x = Indometh)
#' add_to_wb(wb = wb, sheet = 'Iris', x = iris)
#' 
#' @export
add_to_wb <- function(wb = NULL, sheet = NULL, x = NULL, ...) {
    if (!methods::is(wb, "Workbook")) stop("wb must be a Workbook")
    if (!is.character(sheet)) stop("sheet must be a character")
    if (is.null(x))  stop("x can't be NULL")
    openxlsx::addWorksheet(wb = wb, sheetName = sheet)
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = x,
                        ...)
}

#' Save an openxlsx workbook in an excel format quickly
#'
#' @param wb openxlsx workbook
#' @param file xls destination file
#' @examples
#' wb = openxlsx::createWorkbook()
#' add_to_wb(wb = wb, sheet = 'Indometh', x = Indometh)
#' wb_to_xl(wb = wb, file = '/tmp/wb_to_xl.xls')
#' wb_to_xl(wb = wb, file = '/tmp/wb_to_xl.xlsx')
#' @export
wb_to_xl <- function(wb, file = NULL){
    if (!methods::is(wb, "Workbook")) stop("wb must be a Workbook")
    extension <- tolower(tools::file_ext(file))
    stopifnot(extension == 'xls' || extension == 'xlsx')
    xlsx_file <- paste0(tools::file_path_sans_ext(file), '.xlsx')
    openxlsx::saveWorkbook(wb, file = xlsx_file, overwrite = TRUE)
    if ('xls' == extension){
        on.exit(unlink(xlsx_file))
        lbmisc::unoconv(xlsx_file, format = 'xls')
    }
    invisible(NULL)
}


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

#' Read all sheets from an xlsx
#'
#' Read all sheets from an xlsx; return them as a list or assign in the
#' calling environment
#'
#' @param f xlsx file
#' @param ret either list or assign
#' @param ... other options passed to openxlsx::read.xlsx
#'
#' @export
read.xlsx_alls  <- function(f = NULL, ret = c('list', 'assign'), ...){
    ret <- match.arg(ret)
    wb <- openxlsx::loadWorkbook(file = f)
    sheets <- names(wb)
    names(sheets) <- sheets
    res <- lapply(sheets, function(s) openxlsx::read.xlsx(f, sheet = s, ...))
    if (ret == 'list') {
        res
    } else if (ret == 'assign') {
        list2env(res, envir = parent.frame(n = 2))
        invisible(NULL)
    } else stop("Only list or assign return")
}

#' Apply read.table to all the files with a given extension in a given
#' directory
#' 
#' @param dir a directory file to be readed with read.table
#' @param ext which file extension files have to have to be readed
#' @param ret either assign or a list
#' @param ... other options passed to read.table
#' 
#' @export
read.table_dir <- function(dir = 'data/dataset/', 
                           ext = "csv",
                           ret = c('list','assign'),
                           ...)
{
    ret <- match.arg(ret)
    fnames <- list.files(path = dir, pattern = paste0('*.', ext))
    fnames_spl <- strsplit(fnames, "_")
    ## prendi dal terzo al terzultimo per il nome del dataset
    db_names <- tolower(unlist(lapply(fnames_spl, function(x){
        ## browser()
        terzultimo <- length(x) - 3
        paste(x[seq(3, max(3, terzultimo))], collapse = '_')
    })))
    csv_importer <- function(x, verbose = TRUE){
        if (verbose) message("Importing ", x)
        read.table(file = paste0(dir, x), ...)
    }
    rval <- lapply(fnames, csv_importer)
    names(rval) <- db_names
    if (ret == 'assign') {
        list2env(rval, envir = parent.frame(n = 2))
        invisible(NULL)
    } else if (ret == 'list') {
        rval
    } else stop("Only list or assign return")
}
