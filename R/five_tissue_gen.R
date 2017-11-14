#' Generat a five-tissue-type (5TT) segmented tissue image suitable
#' for use in Anatomically-Constrained Tractography (ACT)
#'
#' @param infile The input T1-weighted image
#' @param algorithm algorithm to be used to complete
#' segmentation
#' @param mask Mask for brain
#' @param outfile Output image file
#' @param opts Additional options to pass to function
#' @param verbose Print diagnostic output
#' @param crop crop the resulting 5TT image to reduce its size (changed
#' dimensions compared to input image)
#' @param amyg_hipp_gray Represent the amygdalae and hippocampi
#' as sub-cortical grey matter in the 5TT image
#' @param t2 T2-weighted image in addition to the default T1-weighted image;
#' this will be used as a second input to FSL FAST
#' @param premasked Indicate that brain masking has already
#' been applied to the input image
#' @param lut Manually provide path to the lookup table on
#' which the input parcellation image is based (e.g. FreeSurferColorLUT.txt)
#' @param ... arguments to pass to \code{\link{std_opts}}
#'
#' @return Output filename
#' @export
#'
#' @importFrom neurobase checkimg
#' @examples \dontrun{
#' infile = "~/Downloads/anat+seg.nii.gz"
#' t1 = "~/Downloads/t1.nii.gz"
#' response = five_tissue_gen(t1, algorithm = "freesurfer")
#' }
five_tissue_gen = function(
  infile,
  algorithm = c("fsl", "freesurfer"),
  outfile = NULL,
  crop = TRUE,
  amyg_hipp_gray = FALSE,
  mask = NULL,
  t2 = NULL,
  premasked = FALSE,
  lut = NULL,
  opts = "",
  verbose = TRUE,
  ...
) {


  cmd = "5ttgen"
  cmd = system.file("bin", cmd, package = "mrtrix")

  infile = checkimg(infile)
  algorithm = match.arg(algorithm)

  outfile = check_outfile(
    outfile = outfile,
    retimg = TRUE)

  infile = checkimg(infile)
  if (algorithm == "fsl") {
    # lut only for freesurfer
    if (!is.null(lut)) {
      warning("LUT specified, but algorithm is FSL, LUT not used")
    }
    if (!is.null(t2)) {
      t2 = checkimg(t2)
      opts = c(opts, paste0("-t2 ", t2))
    }
    if (!is.null(mask)) {
      mask = checkimg(mask)
      opts = c(opts, paste0("-mask ", mask))
    }
    if (premasked) {
      opts = c(opts, "-premasked")
    }
  }

  if (algorithm == "freesurfer") {
    # T2/mask only for FSL
    if (!is.null(t2)) {
      warning("T2 specified, but algorithm is Freesurfer, T2 not used")
    }
    if (!is.null(mask)) {
      warning("Mask specified, but algorithm is Freesurfer, Mask")
    }
    if (!is.null(lut)) {
      opts = c(opts, paste0("-lut ", lut))
    }
  }

  if (!crop) {
    opts = c(opts, "-nocrop")
  }
  if (amyg_hipp_gray) {
    opts = c(opts, "-sgm_amyg_hipp")
  }

  opts = std_opts(
    opts = opts,
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
