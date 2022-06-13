#' Install scripts from a package creating symlinks
#'
#' @param package name of the package
#' @param into destination directory of the created symlinks
#' @param location where to search the installed package is
#' @source code from
#'     \url{https://stackoverflow.com/questions/44566100} with minor
#' modifications
#' @examples
#' ## lbmisc::install_scripts(package = 'speeches')
#' @export
install_scripts <- function(package = NULL,
                            into = "~/.local/bin",
                            location = .libPaths()[1],
                            overwrite = FALSE)
{
    # paths
    if (is.null(package))
        stop("Specify the package")
    from_path <- file.path(location, package)
    if (!dir.exists(from_path))
        stop(package, " is not installed in ", location, ". Aborting")
    exec_path <- file.path(from_path, 'exec')
    scripts <- dir(exec_path, full.names = TRUE)

    ## No scripts to be linked
    if (length(scripts) == 0) {
        ## exit but do not raise anything (in order not to stop pake/make)
        ## be quiet
        message("No scripts to be linked in ", exec_path, ". Bypassing.")
        return(invisible(NULL))
    }

    ## Otherwise there are scripts to be linked
    if (!dir.exists(into)) dir.create(into)
    into <- normalizePath(into)
    dests <- file.path(normalizePath(into), basename(scripts))

    # overwrite handling
    if (any(already_exist <- file.exists(dests))) {
        if (overwrite) {
            to_del <- dests[already_exist]
            cat("Deleting existing file: \n\n")
            cat(to_del, sep = "\n")
            cat("\n")
            unlink(to_del)
        } else {
            msg <- sprintf("Skipping '%s': a file by that name already exists",
                           basename(scripts[already_exist]))
            cat(msg)
            scripts <- scripts[!already_exist]
            dests   <-   dests[!already_exist]
        }
    }

    # link creation
    if (length(scripts)) {
        file.symlink(scripts, dests)
        cat("Created the following symlinks:\n\n")
        sl <- paste0(dests, ' -> ', scripts)  
        cat(sl, sep = "\n")
        cat("\n")
    } else
        cat("Nothing installed")

    ## path check
    PATHS <- normalizePath(
        strsplit(Sys.getenv("PATH"), ":", fixed = TRUE)[[1]],
                           mustWork = FALSE)
    if(!into %in% PATHS)
        warning(sprintf("destination '%s' is not on the PATH", into))
    
}
