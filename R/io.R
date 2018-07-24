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

#' Apply read.table to all the files with a given extension in a given
#' directory
#' 
#' @param d a directory file to be readed with read.table
#' @param ... other options passed to read.tables
#' 
#' @export
read.table_dir <- function(d, ...)
{
    fs <- list.files(path = d)
    rval <- read.tables(f = fs, ...)
    rval
}


default_name_gen <- function(x){
    rval <- file_path_sans_ext(tolower(basename(x)))
    gsub(" ", "_", rval)
}

#' Apply read.table to all the files given as parameter and return a
#' list
#' 
#' @param ... options passed to read.table
#' @param name_gen function that return data.frame name given the path
#'     to his file
#' @param verbose if TRUE (by default) print info about progression
#'     (file imported)
#' 
read.tables <- function(f,
                        name_gen = default_name_gen,
                        verbose = TRUE,
                        ...)
{

    table_importer <- function(x, verbose){
        if (verbose) message("Importing ", x)
        read.table(file = x, ...)
    }
    
    rval <- lapply(f, table_importer, verbose = verbose)
    names(rval) <- name_gen(f)
    rval
}


#' Import massively all the data from a xlsx dataset or a directory of
#' text file
#'
#' This is a dispatcher for read.tables, read.table_dir or
#' read.xlsx_alls
#' 
#' @param p char with a path to multiple text data files, a directory
#'     with text data file or a single .xlsx file
#' @param xlsx_params parameters passed to read.xlsx_alls
#' @param text_params arguments passed to read.tables or
#'     read.table_dir
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

    if (all(ext %in% c('csv', 'tsv', 'tab'))){ ## multiple files
        params <- c(list(f = p), text_params)
        do.call(read.tables, params)
    } else if (ext == "xlsx") { ## one xlsx
        params <- c(list(f = p), xlsx_params)
        do.call(read.xlsx_alls, params)
    }  else if (ext == ""){ ## Full directory
        params <- c(list(dir = p), text_params)
        do.call(read.table_dir, params)
    } else stop("p must be a vector of paths to text (csv/tsv/tab) files, ",
                "a single directory or a single xlsx file")
}


#' Export a list of dataset to an xlsx file
#' 
#' @param x openxlsx::write.xlsx x parameter
#' @param f openxlsx::write.xlsx file parameter
#' @param ... further arguments passed to openxlsx::write.xlsx
#' 
#' @export
exporter <- function(x, f, ...) openxlsx::write.xlsx(x = x, file = f, ...)
