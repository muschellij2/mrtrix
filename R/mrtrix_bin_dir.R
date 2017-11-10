#' Directory of Binary Executables for MRTrix
#'
#' @return A path
#' @export
#'
#' @examples
#' mrtrix_bin_dir()
#' file.path(mrtrix_bin_dir(), "dwi2mask")
mrtrix_bin_dir = function() {
  system.file("bin", package = "mrtrix")
}
