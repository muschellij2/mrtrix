#' MRTrix Standard Options
#'
#' @param opts Current options for the function
#' @param verbose Print diagnostic messages.
#' @param debug Display additional debugging information over and above
#' the verbose output
#' @param nthreads number of threads to use for multi-threaded operations
#' @param force force overwrite of output files if pre-existing
#' @param cleanup delete temporary files during script, or
#' temporary directory atscript completion
#' @param use_tempdir logical indicating if this function uses a
#' \code{tempdir} argument
#' @param tempdir Manually specify the path in which to
#' generate the temporary directory
#' @param quiet Suppress all console output during script execution
#'
#' @return Character string
#' @export
#'
#' @examples
#' std_opts()
std_opts = function(
  opts = "",
  verbose = TRUE,
  debug = TRUE,
  nthreads = 0,
  force = FALSE,
  cleanup = TRUE,
  tempdir = tempfile(),
  use_tempdir = TRUE,
  quiet = FALSE) {
  opts =
  if (verbose) {
    opts = c(opts, "-info")
    if (verbose > 1) {
      opts = c(opts, "-debug")
    }
  }
  if (force) {
    opts = c(opts, "-force")
  }
  if (!cleanup) {
    opts = c(opts, "-nocleanup")
  }
  if (!cleanup) {
    opts = c(opts, "-quiet")
  }
  opts = c(opts, paste0("-nthreads ", nthreads))

  if (use_tempdir) {
    if (!dir.exists(tempdir)) {
      dir.create(tempdir)
    }
    opts = c(opts, paste0("-tempdir ", tempdir))
  }
  opts = paste(opts, collapse = " ")
  return(opts)
}
