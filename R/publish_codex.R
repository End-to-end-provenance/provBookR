#' Publish a Provenance Codex
#'
#' Executes an R script, collects its provenance using \code{rdtLite}, and 
#' generates a static SvelteKit digital booklet.
#'
#' @param script The path to the R script to execute.
#' @param output_dir The directory where the static HTML booklet will be generated.
#' @export
publish_codex <- function(script, output_dir = "codex_build") {
  if (!requireNamespace("rdtLite", quietly = TRUE)) {
    stop("Package 'rdtLite' must be installed to use publish_codex().")
  }
  
  if (!file.exists(script)) {
    if (file.exists(basename(script))) {
      script <- basename(script)
    } else if (file.exists(file.path("..", script))) {
      script <- file.path("..", script)
    } else {
      stop("Script not found: ", script)
    }
  }
  
  # Step 1: Run the script and collect provenance
  message("Executing script and collecting provenance...")
  rdtLite::prov.run(script)
  
  prov_dir <- rdtLite::prov.dir()
  json_path <- file.path(prov_dir, "prov.json")
  
  if (is.null(json_path) || !file.exists(json_path)) {
    stop("Failed to generate provenance JSON.")
  }
  prov_json <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  
  # Step 2: Scaffold the codex template
  template_dir <- system.file("codex_template", package = "provBookR")
  if (template_dir == "") {
    stop("Codex template not found in package installation.")
  }
  
  # Create a temporary workspace for the build
  build_ws <- tempfile(pattern = "codex_ws_")
  dir.create(build_ws)
  
  message("Scaffolding SvelteKit codex...")
  files_to_copy <- list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files_to_copy <- files_to_copy[!basename(files_to_copy) %in% c("node_modules", ".svelte-kit", "build")]
  
  file.copy(from = files_to_copy,
            to = build_ws,
            recursive = TRUE,
            copy.mode = TRUE)
  
  # Step 3: Inject the provenance data and script source
  data_dir <- file.path(build_ws, "src", "lib", "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(prov_json, file.path(data_dir, "prov.json"))
  
  script_content <- paste(readLines(script, warn = FALSE), collapse = "\n")
  script_payload <- sprintf('{\n  "name": %s,\n  "code": %s\n}', 
                            jsonlite::toJSON(basename(script), auto_unbox = TRUE),
                            jsonlite::toJSON(script_content, auto_unbox = TRUE))
  writeLines(script_payload, file.path(data_dir, "script_source.json"))
  
  # Step 4: Build the static site
  message("Building static booklet (this requires Node.js)...")
  old_wd <- setwd(build_ws)
  on.exit(setwd(old_wd))
  
  # Use npm install and build
  sys_res_install <- system2("npm", args = c("install"))
  if (sys_res_install != 0) {
    stop("npm install failed. Ensure Node.js and npm are installed.")
  }
  
  sys_res_build <- system2("npm", args = c("run", "build"))
  if (sys_res_build != 0) {
    stop("npm run build failed.")
  }
  
  # Step 5: Copy the final build to the output_dir
  setwd(old_wd)
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE)
  
  message(sprintf("Copying build to %s...", output_dir))
  build_output <- file.path(build_ws, "build")
  file.copy(from = list.files(build_output, full.names = TRUE),
            to = output_dir,
            recursive = TRUE,
            overwrite = TRUE)
  
  message("Codex successfully published at: ", file.path(output_dir, "index.html"))
  return(TRUE)
}

#' Preview a Published Codex Booklet
#'
#' Launches a local web server to preview a published \code{provBook} booklet
#' in the browser without encountering \code{file://} browser CORS restrictions.
#'
#' @param output_dir The directory path containing the published HTML booklet.
#' @param port The port number for the local web server. Default is 8000.
#' @export
preview_codex <- function(output_dir = "codex_build", port = 8000) {
  if (!dir.exists(output_dir)) {
    stop("Directory not found: ", output_dir)
  }
  
  index_path <- file.path(output_dir, "index.html")
  if (!file.exists(index_path)) {
    stop("No index.html found in: ", output_dir)
  }
  
  if (requireNamespace("servr", quietly = TRUE)) {
    servr::httd(dir = output_dir, port = port, browser = TRUE)
  } else {
    message(sprintf("Serving %s on http://localhost:%d ... Press Ctrl+C to stop.", output_dir, port))
    utils::browseURL(sprintf("http://localhost:%d", port))
    system2("python3", args = c("-m", "http.server", as.character(port), "--directory", output_dir))
  }
}
