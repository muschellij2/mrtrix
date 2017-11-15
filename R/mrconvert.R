#' Perform conversion between different file types
#' and optionally extract a subset of the input image
#'
#' @param infile input filename or nifti
#' @param bvecs The file containing the b-vectors.
#' @param bvals The file containing the b-values.
#' @param scheme The file containing a 4xT text file,
#' instead of bvals/bvecs
#' @param datatype specify output image data type
#' @param outfile Output response file
#' @param opts Additional options to pass
#' @param verbose Print diagnostic output
#' @param stride specify the strides of the output data in memory,
#' as comma-separated values
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
#' mrfile = mrconvert(infile, bvals = bvals, bvecs = bvecs,
#' outfile = tempfile(fileext = ".mif"))
#' }
mrconvert = function(
  infile,
  datatype = c("float32", "float32le",
               "float32be", "float64", "float64le", "float64be", "int64", "uint64", "int64le",
               "uint64le", "int64be", "uint64be", "int32", "uint32", "int32le", "uint32le", "int32be",
               "uint32be", "int16", "uint16", "int16le", "uint16le", "int16be", "uint16be", "cfloat32",
               "cfloat32le", "cfloat32be", "cfloat64", "cfloat64le", "cfloat64be", "int8", "uint8",
               "bit"),
  outfile = NULL,
  stride = NULL,
  bvals = NULL,
  bvecs = NULL,
  scheme = NULL,
  opts = "",
  verbose = TRUE,
  ...
) {

  cmd = "mrconvert"
  cmd = system.file("bin", cmd, package = "mrtrix")

  infile = checkimg(infile)

  outfile = check_outfile(
    outfile = outfile,
    retimg = TRUE,
    fileext = ".mif")

  datatype = match.arg(datatype)
  opts = c(opts, paste0("-datatype ", datatype))

  scheme_opts = scheme_or_bvals(
    bvals = bvals,
    bvecs = bvecs,
    scheme = scheme)
  opts = c(opts, scheme_opts)


  if (!is.null(stride)) {
    stride = paste(stride, collapse = ",")
    opts = c(opts, paste0("-stride ", stride))
  }

  opts = std_opts(opts = opts,
                  verbose = verbose,
                  use_tempdir = FALSE, # no tempdir available
                  ...)

  opts = paste(opts, collapse = " ")

  cmd = paste(cmd, opts,
              infile, outfile)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd)
  if (res != 0) {
    warning("Result not zero, may be a problem!")
  }
  return(outfile)
}
