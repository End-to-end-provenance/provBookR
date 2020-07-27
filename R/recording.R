#' Provenance recorder for provBookR
#'
#' \code{prov.recording}
#' @title provenance recorder
#' @param file.name name of provenance file
#' @export
prov.recording <- function(file.name) {

  # Alert the user about the provenance collection process
  message(paste("Collecting provenance for ", file.name, "..."))

  # Collect the provenance specifying the maximum snapshot size
  rdtLite::prov.run(file.name, prov.dir = ".", overwrite = FALSE, snapshot.size = Inf)

  # Create a directory to store the collected provenance
  dir.create("provenance")

  # Collect the generated provenance directories
  # Only collect the folders that have "prov_file.name_"
  # This allows provBook to check all the current and previously collected provenance directories
  prov.dir.names <- list.files(pattern = "^prov_")
  path.to.prov.dir <- prov.dir.names[length(prov.dir.names)] # Gets the last datetime
  path.to.json <- paste(getwd(), "/provenance/", path.to.prov.dir, "/prov.json", sep = "")
  file.copy(path.to.prov.dir, "provenance", recursive = TRUE, overwrite = TRUE)

  # Return the user the path to the created provenance
  return(path.to.json)
}
