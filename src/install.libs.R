folders = c("bin", "lib", "core", "src", "docs")
dest <- file.path(R_PACKAGE_DIR, folders)

sapply(dest, dir.create, recursive = TRUE, showWarnings = FALSE)
mapply(function(x, y) {
  file.copy(x, y, overwrite = TRUE, recursive = TRUE)
}, folders, dest)
