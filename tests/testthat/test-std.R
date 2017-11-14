test_that("standard options", {

  initial = "-nthreads 0"
  expect_equal(std_opts(use_tempdir = FALSE), initial)

  tdir = tempfile()
  res = std_opts(use_tempdir = TRUE, tempdir = tdir)
  expect_equal(res, paste(initial, "-tempdir", tdir))

  # make sure the directory was made
  expect_true(dir.exists(tdir))
})
