#' save a figure done in R to a file with multiple formats
#'
#' save a figure done in R to a file with multiple formats
#' 
#' @param code code to be evaluated (as string) to produce the figure
#' @param outfile path without extension
#' @examples
#'
#' fig_dump("roc_with_boxplot(sani_mci$score, sani_mci$sani_mci)", outfile = outfile)
#'
#' @export
fig_dump <-
    function(code,
             outfile = "outputs/test"## ,
             ## width   = inches(cm = 8.5),
             ## height  = inches(cm = 8.5),
             ## dpi     = 600
             )
{
    outpdf <- sprintf("%s.pdf", outfile)
    tmp <- capture.output({
        ## pdf(outpdf, width = width, height=height) #croda tutto con roc_with_boxplot
        pdf(outpdf)
        eval(parse(text=code))
        dev.off()
    })
    outeps <- sprintf("%s.eps", outfile)
    tmp <- capture.output({
        ## cairo_ps(outeps, width = width, height=height) # croda tutto con roc_with_boxplot
        cairo_ps(outeps)
        eval(parse(text=code))
        dev.off()
    })
    outpng <- sprintf("%s.png", outfile)
    tmp <- capture.output({
        ## png(outpng, width=width, height=height, units="in",res=dpi)# croda tutto con roc_with_boxplot
        png(outpng)
        eval(parse(text=code))
        dev.off()
    })
}


#' include a figure/includegraphics from a file in Latex code 
#'
#' include the figure/includegraphics from a file in Latex code
#' (for an image produced in R pythontex)
#' 
#' @param path path to the pdf file
#' @param label label of the latex figure
#' @param caption caption of the latex figure
#' @param scale used for re-dimensioning (defaul = 1)
#' @examples
#'
#' include_figure(path_sans_ext = "/tmp/test",
#'              label = "test_r",
#'              caption = 'test R figure')
#'
#' @export
include_figure <- function(path, label, caption, scale = 1){
    path_clean <- gsub(".pdf", "", path)
    include_line <- sprintf(
        "\\includegraphics[scale=%.2f]{%s}", scale, path_clean
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

