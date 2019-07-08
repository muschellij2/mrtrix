#' Estimate fibre orientation distributions
#' from diffusion data using spherical deconvolution
#'
#' @param infile the input DWI image containing volumes
#' that are both diffusion weighted and b=0
#' @param count count number of tracks in file explicitly, ignoring the header
#' @param ... arguments to pass to \code{\link{std_opts}}
#'
#' @return Output filename
#' @export
#'
#' @importFrom neurobase checkimg check_outfile
#' @examples \dontrun{
#' infile = "~/Downloads/data.nii.gz"
#' bvals = "~/Downloads/bvals"
#' bvecs = "~/Downloads/bvecs"
#' fod = tckinfo(infile, response = response,
#' bvals = bvals, bvecs = bvecs,
#' algorithm = "csd")
#'
#' }
tckinfo = function(
  infile,
  counts = TRUE,
  outfile = NULL,
  opts = "",
  verbose = TRUE,
  ...
) {

  cmd = "tckinfo"
  cmd = system.file("bin", cmd, package = "mrtrix")

  infile = checkimg(infile)

  outfile = check_outfile(
    outfile = outfile,
    retimg = TRUE)

  if (counts) {
    opts = c(opts, "-count")
  }

  opts = std_opts(opts = opts,
                  verbose = verbose,
                  use_tempdir = FALSE, # no tempdir available
                  ...)

  opts = paste(opts, collapse = " ")

  cmd = paste(cmd, opts, infile)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd, intern = TRUE)
  # if (res != 0) {
  #   warning("Result not zero, may be a problem!")
  # }
  return(res)
}
