#' dump help documentation to a file
#' 
#'@export
dump_doc <- function(topic="lm", package="stats", outfile = "/tmp/doc.txt"){
  rd_file <- help((topic), package = (package))
  tools:::Rd2txt(utils:::.getHelpFile(rd_file), out=outfile)
}
