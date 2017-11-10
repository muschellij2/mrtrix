folnames = c("bin", "lib", "core", "src", "docs")
folders = file.path("mrtrix3", folnames)
# dest <- file.path(R_PACKAGE_DIR, folders)

# sapply(dest, dir.create, recursive = TRUE, showWarnings = FALSE)
sapply(folders, function(x) {
  file.copy(x, to = R_PACKAGE_DIR, overwrite = TRUE, recursive = TRUE)
})
# }, folders, dest)

