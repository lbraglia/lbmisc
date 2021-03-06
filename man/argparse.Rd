% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argparse.R
\name{argparse}
\alias{argparse}
\title{Simple command line argument parser}
\usage{
argparse(line = commandArgs(trailingOnly = TRUE), opts = NULL)
}
\arguments{
\item{opts}{a list of lists, each of one has tree argument: 'name'
(a character eg 'example' is for --example), 'default' (a value
if nothing is specified at command line, eg TRUE), 'f' (the
coercer function to assure the input is validated and of the
proper type)}
}
\description{
Simple command line argument parser: returns a named list with
arguments and values
}
\examples{
## Beginning of an Rscript
#!/usr/bin/Rscript --vanilla --quiet

# No opts, no validation
args <- lbmisc::argparse()

# With opts, validation
opts <- list(
   list(name = 'a_character' , default = "hello" , f = as.character),
   list(name = 'a_logical'   , default = "TRUE"  , f = as.logical),
   list(name = 'an_integer'  , default = "1L"    , f = as.integer),
   list(name = 'a_numeric'   , default = "1.5"   , f = as.numeric)
)
args <- lbmisc::argparse(opts = opts)
## print the effect of validation
print(str(args))

## with from the command line
## rscript --a_character --a_logical FALSE --an_integer 2 --a_numeric

}
