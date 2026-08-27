test_that("preview_codex validates output directory existence", {
  fake_dir <- file.path(tempdir(), paste0("nonexistent_dir_", as.numeric(Sys.time())))
  expect_error(
    preview_codex(output_dir = fake_dir),
    "Directory not found"
  )
})

test_that("preview_codex checks for presence of index.html", {
  empty_dir <- file.path(tempdir(), paste0("empty_codex_", as.numeric(Sys.time())))
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_error(
    preview_codex(output_dir = empty_dir),
    "No index.html found"
  )
})
