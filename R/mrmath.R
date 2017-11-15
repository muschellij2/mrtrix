#' Compute summary statistic on image intensities either across images, or
#' along a specified axis of a single image
#'
#' @param infile input filename or filenames.  Can be list of nifti objects too.
#' @param datatype specify output image data type
#' @param operation Supported operations are:
#' mean, median, sum, product, rms (root-mean-square value),
#' norm (vector 2-norm), var (unbiased variance),
#' std (unbiased standard deviation), min,
#' max, absmax (maximum absolute value),
#' magmax (value with maximum absolute
#' value, preserving its sign).
#' @param outfile Output response file
#' @param opts Additional options to pass
#' @param axis perform operation along a specified axis of a single input image
#' @param verbose Print diagnostic output
#' @param ... arguments to pass to \code{\link{std_opts}}
#'
#' @return Output filename
#' @export
#'
#' @importFrom neurobase checkimg check_outfile
#' @examples \dontrun{
#' infile = "~/Downloads/data.nii.gz"
#' }
mrmath = function(
  infile,
  operation = c("mean", "median", "sum",
                "product", "rms", "norm", "var", "std",
                "min", "max", "absmax", "magmax"),
  datatype = c("float32", "float32le",
               "float32be", "float64", "float64le", "float64be", "int64", "uint64", "int64le",
               "uint64le", "int64be", "uint64be", "int32", "uint32", "int32le", "uint32le", "int32be",
               "uint32be", "int16", "uint16", "int16le", "uint16le", "int16be", "uint16be", "cfloat32",
               "cfloat32le", "cfloat32be", "cfloat64", "cfloat64le", "cfloat64be", "int8", "uint8",
               "bit"),
  outfile = NULL,
  axis = NULL,
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
    fileext = ".nii.gz")

  operation = match.arg(operation)

  datatype = match.arg(datatype)
  opts = c(opts, paste0("-datatype ", datatype))

  if (!is.null(axis)) {
    opts = c(opts, paste0("-axis ", axis))
  }

  opts = std_opts(opts = opts,
                  verbose = verbose,
                  use_tempdir = FALSE, # no tempdir available
                  ...)

  opts = paste(opts, collapse = " ")

  # can be multiple
  infile = paste(infile, collapse = " ")

  cmd = paste(cmd, opts,
              infile, operation, outfile)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd)
  if (res != 0) {
    warning("Result not zero, may be a problem!")
  }
  return(outfile)
}
