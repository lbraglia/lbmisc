#' print the figure/includegraphics Latex code 
#'
#' print the figure/includegraphics Latex code for
#' an image produced in R in pythontex
#' 
#' @param path_sans_ext path to the pdf file without extension (".pdf")
#' @param label label of the latex figure
#' @param caption caption of the latex figure
#' @param scale used for re-dimensioning (defaul = 1)
#' @examples
#'
#' latex_figure(path_sans_ext = "/tmp/test",
#'              label = "test_r",
#'              caption = 'test R figure')
#'
#' @export
latex_figure <- function(path_sans_ext, label, caption, scale = 1){
    include_line <- sprintf(
        "\\includegraphics[scale=%.2f]{%s}", scale, path_sans_ext
    )
    label_line <-  sprintf("\\label{fig:%s}", label)
    caption_line <- sprintf("\\caption{%s}",caption)
    lines <- c(
        "\\begin{figure}",
        "\\centering",
        include_line,
        caption_line,
        label_line,
        "\\end{figure}")
    cat(lines, sep = "\n")
}
