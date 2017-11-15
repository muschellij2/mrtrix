#' Transform DWI to mask
#'
#' @param infile the input DWI image containing volumes
#' that are both diffusion weighted and b=0
#' @param bvecs The file containing the b-vectors.
#' @param bvals The file containing the b-values.
#' @param scheme The file containing a 4xT text file,
#' instead of bvals/bvecs
#' @param outfile output mask file name
#' @param opts Additional options to pass
#' @param verbose Print diagnostic output
#' @param clean_scale amount of cleaning of the mask is done
#'
#' @return Name of output file
#' @export
#'
#' @importFrom neurobase checkimg
#' @examples \dontrun{
#' infile = "~/Downloads/data.nii.gz"
#' bvals = "~/Downloads/bvals"
#' bvecs = "~/Downloads/bvecs"
#' mask = dwi2mask(infile, bvals = bvals, bvecs = bvecs)
#' }
dwi2mask = function(
  infile,
  bvals = NULL,
  bvecs = NULL,
  scheme = NULL,
  outfile = NULL,
  clean_scale = 2,
  opts = "",
  verbose = TRUE) {

  cmd = "dwi2mask"

  cmd = system.file("bin", cmd, package = "mrtrix")

  infile = checkimg(infile)

  outfile = check_outfile(
    outfile = outfile,
    retimg = TRUE)

  scheme_opts = scheme_or_bvals(
    bvals = bvals,
    bvecs = bvecs,
    scheme = scheme)
  opts = c(opts, scheme_opts)
  if (verbose) {
    opts = c(opts, "-info")
  }

  if (!is.null(clean_scale)) {
    opts = c(opts,
             paste0("-clean_scale ", clean_scale))
  }

  opts = paste(opts, collapse = " ")

  cmd = paste(cmd, opts, infile, outfile)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd)
  if (res != 0) {
    warning("Result not zero, may be a problem!")
  }
  return(outfile)

}

#' @rdname dwi2mask
#' @export
scheme_or_bvals = function(
  bvals = NULL,
  bvecs = NULL,
  scheme = NULL) {

  have_bvals = !is.null(bvals)
  have_bvecs = !is.null(bvecs)
  bb = have_bvals || have_bvecs
  schemer = !is.null(scheme)

  opts = ""
  if (bb && schemer) {
    stop(paste0("Only scheme or bvals/bvecs ",
                "need to be specified"))
  }
  if (bb) {
    bb = have_bvals && have_bvecs
    if (!bb) {
      stop("If bvals/bvecs specified, both must be!")
    }
    if (is.matrix(bvecs)) {
      stopifnot(ncol(bvecs) == 3)
      tfile = tempfile(fileext = ".txt")
      bvecs = apply(bvecs, 1, paste, collapse = " ")
      writeLines(bvecs, con = tfile)
      bvecs = tfile
    }

    if (is.numeric(bvals)) {
      tfile = tempfile(fileext = ".txt")
      bvals = as.character(bvals)
      writeLines(bvals, con = tfile)
      bvals = tfile
    }
    bvals = normalizePath(bvals, winslash = "/")
    bvecs = normalizePath(bvecs, winslash = "/")
    bvecs = shQuote(bvecs)
    bvals = shQuote(bvals)
    opts = paste0("-fslgrad ", bvecs, " ", bvals)
  }

  if (schemer) {
    if (is.matrix(scheme)) {
      stopifnot(ncol(scheme) == 3)
      tfile = tempfile(fileext = ".txt")
      scheme = apply(scheme, 1, paste, collapse = " ")
      writeLines(scheme, con = tfile)
      scheme = tfile
    }
    scheme = shQuote(scheme)
    opts = paste0("-grad ", scheme)
  }
  return(opts)
}
