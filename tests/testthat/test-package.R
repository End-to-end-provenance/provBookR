test_that("package exports expected functions", {
  ns_exports <- getNamespaceExports("provBookR")
  expect_true("publish_codex" %in% ns_exports)
  expect_true("preview_codex" %in% ns_exports)
})

test_that("codex template contains required assets and configuration", {
  template_dir <- system.file("codex_template", package = "provBookR")
  if (template_dir == "" || !dir.exists(template_dir)) {
    template_dir <- testthat::test_path("../../inst/codex_template")
  }
  
  expect_true(dir.exists(template_dir))
  expect_true(file.exists(file.path(template_dir, "package.json")))
  expect_true(file.exists(file.path(template_dir, "svelte.config.js")))
  expect_true(dir.exists(file.path(template_dir, "src", "lib", "components")))
})
