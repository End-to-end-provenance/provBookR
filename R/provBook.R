#' Main provBookR interface
#'
#' @name provBook
#' @return full provBookR browser or enable lite operation
#' @param file.name provenance file name
#' @param mode type of browsing
#' @export
provBookR <- function(file.name, mode) {

  # Collect the user input mode
  switch(tolower(mode),
    "lite" = provBookR.lite(file.name), # "lite" mode
    "full" = provBookR.prov.browse(file.name), # "full" mode
    "default" = provBookR.prov.browse(file.name) # Default: "full" mode
  )
}

# "provBookRLite"
provBookR.lite <- function(file.name) {

  # Ask user for data.object.name
  message("variable name?: ")
  data.object.name <- readline(prompt = "provBookR.lite> ")

  # Alert user
  message(paste("Running provBookR lite for ", data.object.name, " ", "from ", file.name, "...", sep = ""))

  # Check the file type of file.name
  if (json.check(file.name)) {
    
    # Query provenance
    provBookR.prov.querying(file.name, data.object.name, "BO")
  } else {
    
    # Record provenance
    recorded.prov <- provBookR.prov.recording(file.name)
    
    # Query provenance
    provBookR.prov.querying(recorded.prov, data.object.name, "BO")
  }

  message("Generate a provbook for this object history? (Y/N):") # Output message to the user
  user.input <- tolower(readline(prompt = "provBookR> ")) # Collect user input
  if (user.input == "y") {

    # Generate provbook
    # Call provbook generator
    # UI.user.input.BG(prov.json, data.object.name)
    message("Done!")
    cat(paste("provbook for variable ", "\"", data.object.name, "\"", " is stored in ", getwd(), sep = ""))
  } else {
    stop("closing provBookr.lite...") # Closing provBookR.lite()
  }
}

# Check if the input file is a .json or .R file
json.check <- function(file.name) {
  message("Checking file extension...") # Output message to the user
  if (grepl("\\.json", file.name)) {
    return(TRUE) # The file is a .json
  }
  else if (grepl("\\.r", file.name) | grepl("\\.R", file.name) | grepl("\\.rmd", file.name)) {
    return(FALSE) # The file is a .R file
  } else {
    stop("This is neither a .JSON, .R, or .RMD file") # Dude...give me the correct input, please
  }
}
