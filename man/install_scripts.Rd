% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_scripts.R
\name{install_scripts}
\alias{install_scripts}
\title{Install scripts from a package creating symlinks}
\source{
code from
    \url{https://stackoverflow.com/questions/44566100} with minor
modifications
}
\usage{
install_scripts(
  package = NULL,
  into = "~/.local/bin",
  location = .libPaths()[1],
  overwrite = FALSE
)
}
\arguments{
\item{package}{name of the package}

\item{into}{destination directory of the created symlinks}

\item{location}{where to search the installed package is}
}
\description{
Install scripts from a package creating symlinks
}
\examples{
## lbmisc::install_scripts(package = 'speeches')
}
