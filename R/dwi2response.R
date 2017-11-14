#' Estimate response function(s) for spherical deconvolution
#'
#' @param infile the input DWI image containing volumes
#' that are both diffusion weighted and b=0
#' @param algorithm algorithm to be used to complete
#' response estimation
#' @param bvecs The file containing the b-vectors.
#' @param bvals The file containing the b-values.
#' @param scheme The file containing a 4xT text file,
#' instead of bvals/bvecs
#' @param mask initial mask for response voxel selection
#' @param lmax The maximum harmonic degree(s) of response function
#' estimation (single value for single-shell response,
#' comma-separated values for multi-shell response)
#' @param shells The b-value shell(s) to use in response
#' function estimation (single value for single-shell response,
#' comma-separated values for multi-shell response)
#' @param outfile Output response file
#' @param opts Additional options to pass
#' @param verbose Print diagnostic output
#' @param ... arguments to pass to \code{\link{std_opts}}
#'
#' @return Output filename
#' @export
#'
#' @importFrom neurobase checkimg
#' @examples \dontrun{
#' infile = "~/Downloads/data.nii.gz"
#' bvals = "~/Downloads/bvals"
#' bvecs = "~/Downloads/bvecs"
#' response = dwi2response(infile, bvals = bvals, bvecs = bvecs)
#' }
dwi2response = function(
  infile,
  algorithm = c("tournier", "dhollander",
                "msmt_5tt", "fa", "tax", "manual"),
  bvals = NULL,
  bvecs = NULL,
  scheme = NULL,
  mask = NULL,
  lmax = NULL,

  shells = NULL,
  outfile = tempfile(),
  opts = "",
  verbose = TRUE,
  ...
) {

  cmd = "dwi2response"
  cmd = system.file("bin", cmd, package = "mrtrix")

  infile = checkimg(infile)
  algorithm = match.arg(algorithm)

  scheme_opts = scheme_or_bvals(
    bvals = bvals,
    bvecs = bvecs,
    scheme = scheme)
  opts = c(opts, scheme_opts)

  if (!is.null(mask)) {
    mask = checkimg(mask)
    opts = c(opts, paste0("-mask ", mask))
  }

  if (!is.null(lmax)) {
    lmax = paste(lmax, collapse = ",")
    opts = c(opts, paste0("-lmax ", lmax))
  }

  if (!is.null(shells)) {
    shells = paste(shells, collapse = ",")
    opts = c(opts, paste0("-shell ", shells))
  }

  opts = c(opts, paste0("-nthreads ", nthreads))

  opts = std_opts(opts = opts,
                  verbose = verbose,
                  use_tempdir = TRUE,
                  ...)


  cmd = paste(cmd, algorithm, opts, infile, outfile)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd)
  if (res != 0) {
    warning("Result not zero, may be a problem!")
  }
  return(outfile)
}
