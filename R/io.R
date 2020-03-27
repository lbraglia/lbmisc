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

#' print as xtable and add to workbook
#' 
#' print as xtable and add to workbook
#' 
#' @param x the object
#' @param wb add_to_wb workbook name
#' @param sheet add_to_wb sheet name
#' @param label xtable::xtable label
#' @param caption xtable::xtable caption
#' @param xtable_par xtable::xtable further parameters
#' @param print_xtable_par xtable::print.xtable further parameters
#' @param add_to_wb_par add_to_wb further parameters
#' 
#' @examples \dontrun{
#' xtp_atwb(Indometh,
#'          wb = wb, sheet = 'Indometh', label = 'tab:indometh',
#'          caption = 'Indometh dataset')
#' }
#' @export
xtp_atwb <- function(x, wb = NULL, sheet = '', label = '', caption = '',
                    xtable_par = list(digits = 3),
                    print_xtable_par = list(include.rownames = TRUE),
                    add_to_wb_par = list(rowNames = TRUE)
                    ){
    xtable_par <- c(list(x = x, caption = caption, label = label), xtable_par)
    xt <- do.call(xtable::xtable, xtable_par)
    print_xtable_par <- c(list(x = xt), print_xtable_par)
    do.call(xtable::print.xtable, print_xtable_par)
    add_to_wb_par <- c(list(wb = wb, sheet = sheet, x = x), add_to_wb_par)
    do.call(add_to_wb, add_to_wb_par)
    invisible(NULL)
}




#' Save an openxlsx workbook in an excel format quickly
#'
#' @param wb openxlsx workbook
#' @param file xls destination file
#' @examples
#' \dontrun{
#' wb = openxlsx::createWorkbook()
#' add_to_wb(wb = wb, sheet = 'Indometh', x = Indometh)
#' wb_to_xl(wb = wb, file = '/tmp/wb_to_xl.xls')
#' }
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

    ## now ext can be one of csv/tsv/tab, xlsx, zip or "" (for a
    ## directory)

    ## for each zip, unzip it in tempdir and add tempdir to p
    zip_format <- c('zip', 'ZIP')
    if (any(zip_format %in% ext)) {
        zipfiles <- p[ext %in% zip_format]
        ## td <- tempdir(check = TRUE)
        td <- tempdir()
        on.exit(unlink(paste0(td, "/*"), recursive = TRUE, force = TRUE))
        lapply(zipfiles, function(z) unzip(z, exdir = td))
        p <- c(p, td) %without% zipfiles
        ext <- tools::file_ext(p)
    }
    
    ## let's normalize the directory modifying p and ext
    ## to point to the file there available
    if ('' %in% ext) {
        p <- unlist(lapply(p, function(x) {
            if (tools::file_ext(x) == "") {
                paste(x, list.files(path = x), sep = '/')
            } else x
        }))
        ext <- tools::file_ext(p)
    }
    
    ## now they should all be c('csv', 'tsv', 'tab', 'xlsx')
    filepaths        <- p
    names(filepaths) <- file_path_sans_ext(basename(filepaths))
    ## do proper functions call based on file extension
    rval <- lapply(filepaths, function(fp) {
        message("Processing ", fp)
        ext <- tools::file_ext(fp)
        if (ext %in% c('csv', 'tsv', 'tab')){
            do.call(read.table, c(list('file' = fp), text_params))
        } else if (ext %in% 'xlsx') {
            do.call(read.xlsx_alls, c(list('f' = fp), xlsx_params))
        } else message("Skipping", fp, ": not a csv, tsv, tab or xlsx")
    })

    ## now we could have a list of list of data.frame (if we imported
    ## multiple sheets xlsx files): flatten to uniform to a single list
    ## of data.frames. Handle naming sensibly
    
    uniform <- function(x, n) {
        if (is.data.frame(x)) {
            setNames(list(x), n)
        } else setNames(x, paste(n, names(x), sep = "_"))
    }
    rval <- Map(uniform, rval, as.list(names(rval)))
    rval <- Reduce(c, x = rval, init = list())
    names(rval) <- gsub(" ", "_", names(rval))
    rval

}



#' Export a list of dataset to an xlsx file
#' 
#' @param x openxlsx::write.xlsx x parameter
#' @param f openxlsx::write.xlsx file parameter
#' @param ... further arguments passed to openxlsx::write.xlsx
#' 
#' @export
exporter <- function(x, f, ...) openxlsx::write.xlsx(x = x, file = f, ...)

#' Download docs from Google Drive easily
#'
#' Download docs from Google Drive easily  (given file_id and destfile)
#' 
#' @param file_id google file id (see the url)
#' @param destfile destination file
#' @source https://stackoverflow.com/questions/37453841
#' 
#' @export
gdrive_download <- function(file_id,  destfile){
    url <- paste0("https://docs.google.com/uc?export=download&id=", file_id)
    download.file(url = url, destfile = destfile)
    invisible(NULL)
}
