#' A maxima engine for knitr
#'
#' A maxima engine for knitr
#' @param options knitr options
#' @export
#' @examples \dontrun{
#' ## Add this to your document
#' library(knitr)
#' knit_engines$set(maxima = yapomif::eng_maxima)
#' opts_chunk$set("engine" = "maxima")
#' opts_chunk$set("echo" = FALSE)
#' }
eng_maxima <- function(options) {

  engine <- options$engine
  code <- paste("--very-quiet -batch-string",
                shQuote(paste(options$code, collapse = '\n')))
  code <- paste(options$engine.opts, code)
  cmd <- if(is.null(options$engine.path)) engine else options$engine.path
  out <- if (options$eval) {
    message('running: ', cmd, ' ', code)
    tryCatch(system2(cmd, code, stdout = TRUE, stderr = TRUE), error = function(e) {
      if (!options$error) stop(e)
      paste('Error in running command', cmd)
    })
  } else ''
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out, 'status')))
    stop(paste(out, collapse = '\n'))
  knitr::engine_output(options, options$code, out)

}
