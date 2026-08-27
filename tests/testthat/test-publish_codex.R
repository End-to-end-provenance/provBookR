test_that("publish_codex throws error for missing script", {
  fake_script <- file.path(tempdir(), "non_existent_fake_script.R")
  expect_error(
    publish_codex(fake_script),
    "Script not found"
  )
})

test_that("publish_codex builds a valid digital codex from an R script", {
  # Skip on CRAN and CI runners to keep R CMD check fast and avoid external network/node build issues
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("rdtLite")
  skip_if_not(nzchar(Sys.which("npm")), "npm binary not found in system PATH")

  # Create a self-contained minimal R test script
  test_script <- tempfile(pattern = "test_script_", fileext = ".R")
  writeLines(c(
    "# Minimal test analysis script",
    "x <- 1:10",
    "y <- x * 2.5",
    "total <- sum(y)",
    "saveRDS(total, file.path(tempdir(), 'test_output.rds'))"
  ), test_script)
  on.exit(unlink(test_script), add = TRUE)

  # Destination directory for test build
  test_out_dir <- tempfile(pattern = "test_codex_build_")
  on.exit(unlink(test_out_dir, recursive = TRUE), add = TRUE)

  # Publish codex
  result <- publish_codex(script = test_script, output_dir = test_out_dir)

  expect_true(result)
  expect_true(dir.exists(test_out_dir))
  expect_true(file.exists(file.path(test_out_dir, "index.html")))
  
  # Check that index.html is non-empty
  index_info <- file.info(file.path(test_out_dir, "index.html"))
  expect_gt(index_info$size, 0)
})
