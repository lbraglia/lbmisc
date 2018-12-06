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
#' @param xname name to be given to the dataset
#' @export
write_sas <- function(x = NULL, file = NULL, xname = NULL){

    if (is.null(xname)) xname <- deparse(substitute(x))
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
#' @param ... other options passed to openxlsx::read.xlsx
#'
#' @export
read.xlsx_alls  <- function(f = NULL, ...){
    wb <- openxlsx::loadWorkbook(file = f)
    sheets <- names(wb)
    names(sheets) <- sheets
    res <- lapply(sheets, function(s) openxlsx::read.xlsx(f, sheet = s, ...))
    res
}

#' Import massively all the data from a xlsx dataset or a directory of
#' text file
#'
#' This is a dispatcher for read.tables, read.table_dir or
#' read.xlsx_alls
#' 
#' @param p char with a path to multiple text data files, a directory
#'     with text data file or a single .xlsx file
#' @param xlsx_params parameters passed to read.xlsx_alls (they will
#'     be commonly applied to all xlsx)
#' @param text_params arguments passed to read.table (they will be
#'     commonly applied to all text files)
#' 
#' @export
importer <- function(p,
                     xlsx_params = list(),
                     text_params = list(
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         sep = ';',
                         dec = '.',
                         quote = "\"",
                         fill = TRUE))
{
    ext <- tools::file_ext(p)

    if (all(ext %in% c('csv', 'tsv', 'tab', 'xlsx'))){
        ## multiple single files: copy them in a temporary dir
        ## and start directory importer
        d <- tempdir()
        on.exit(unlink(d, recursive = TRUE, force = TRUE))
        file.copy(p, to = d)
        importer(p = d, xlsx_params = xlsx_params, text_params = text_params)
    } else if (ext == "zip") {
        ## unzip in a directory and start directory importer (recursion)
        d <- tempdir()
        on.exit(unlink(d, recursive = TRUE, force = TRUE))
        unzip(p, exdir = d)
        importer(p = d, xlsx_params = xlsx_params, text_params = text_params)
    }  else if (ext == ""){
        ## Directory case
        ## obtain single file infos
        files <- list.files(path = p)
        filepaths <- paste(p, files, sep = '/')
        names(filepaths) <- file_path_sans_ext(files)
        ## do proper functions call based on file extension
        rval <- lapply(filepaths, function(fp) {
            ext <- tools::file_ext(fp)
            if (ext %in% c('csv', 'tsv', 'tab')){
                do.call(read.table, c(list('file' = fp), text_params))
            } else if (ext %in% 'xlsx') {
                do.call(read.xlsx_alls, c(list('f' = fp), xlsx_params))
            } else stop ("All files must be a csv, tsv, tab or xlsx")
        })
        ## uniform to a single list of data.frames with sensible naming
        uniform <- function(x, n) {
            if (is.data.frame(x)) {
                setNames(list(x), n)
            } else setNames(x, paste(n, names(x), sep = "_"))
        }
        rval <- Map(uniform, rval, as.list(names(rval)))
        rval <- Reduce(c, x = rval, init = list())
        names(rval) <- gsub(" ", "_", names(rval))
        rval
        
    } else {
        msg <- paste0("p must be a vector of paths csv/tsv/tab/xlsx files, ",
                      "a zip file or a directory containing the same formats")
        stop(msg)
    }
}



#' Export a list of dataset to an xlsx file
#' 
#' @param x openxlsx::write.xlsx x parameter
#' @param f openxlsx::write.xlsx file parameter
#' @param ... further arguments passed to openxlsx::write.xlsx
#' 
#' @export
exporter <- function(x, f, ...) openxlsx::write.xlsx(x = x, file = f, ...)
