#' Provenance browser
#'
#' @name browser
#' @return provBookR browser
#' @param file.name provenance file name
#' @importFrom provParseR prov.parse
#' @importFrom provParseR get.data.nodes
#' @export
provBookR.prov.browse <- function(file.name) {

  # Begin initializing browser
  initializing.browser.message <- "provBookR browser initializing..."
  message(initializing.browser.message) # Output message to the user

  # Check the file type of file.name
  if (json.check(file.name)) {

    # Start browser
    prov.browser(file.name)
  } else {

    # Record provenance
    recorded.prov <- provBookR.prov.recording(file.name) # outputs a .json

    # Start browser
    prov.browser(recorded.prov)
  }
}

#' @rdname browser
#' @aliases browser
#' @description browser
#' @return provbook options
#' @export
prov.browser <- function(file.name) {

  # Output message to the user
  message("provBookR browser initialized!")

  # Start browser loop...
  UI.condition <- TRUE
  while (UI.condition) {

    # Let the user know the browser is running
    initial.options.message <- "provBookR browser running, type \"help\" for more information or Q to quit"
    initial.options.prompt <- paste(initial.options.message, file.name, sep = "\n")
    message(initial.options.prompt) # Output message to the user

    # Collecting initial user input
    user.input <- readline(prompt = "provBookR> ")
    user.input.strip <- strsplit(user.input, "\\s+")[[1]]
    user.input.command <- user.input.strip[1]
    data.object.name <- user.input.strip[2]
    if (user.input.command == "q" | user.input.command == "Q" | tolower(user.input.command) == "quit") {

      # Call provbook quit menu
      UI.condition <- UI.user.input.quit()
    } else if (user.input.command == "help" | user.input.command == "h" | user.input.command == "?") {

      # Call provbook help menu
      UI.user.input.help()
    } else if (user.input.command == "ls" | user.input.command == "LS") {

      # Print the nodes
      print.nodes(file.name)
    } else if (user.input.command == "ao" | user.input.command == "AO") {

      # Call provbook forweard lineage
      UI.user.input.AO(file.name, data.object.name, "AO")
    } else if (user.input.command == "bo" | user.input.command == "BO") {

      # Call provbook backward lineage
      UI.user.input.BO(file.name, data.object.name, "BO")
    } else if (user.input.command == "s" | user.input.command == "S") {

      # Call provSummarizeR
      UI.user.input.S(file.name)
    } else if (user.input.command == "db" | user.input.command == "DB") {

      # Call provDebugR
      UI.user.input.DB(file.name)
    } else if (user.input.command == "c" | user.input.command == "C" | tolower(user.input.command) == "clean") {

      # Call Rclean
      UI.user.input.C(file.name)
    } else {

      # Output message to the user
      message(paste("command not supported"), sep = "\n")
    }
  }
}

#' @rdname browser
#' @aliases browser
UI.user.input.quit <- function() {

  # Alert user about their quit options
  message("Are you sure you want to quit? (Y/N):")
  user.input <- tolower(readline(prompt = "provBookR> ")) # Collect user input
  if (user.input == "y") {
    return(FALSE) # quit provBookR.browser()
  } else {
    return(TRUE) # continue provBookR.browser()
  }
}

#' @rdname browser
#' @aliases browser
UI.user.input.help <- function() {

  # Create user "help menu"
  cat("provBookR(operations)> [command][space][variable.name]:\n")
  options <- c(
    "Quit provBookR", "provBookR operations", "List R objects", "Show me how R object was created",
    "Show me what this R object was used to create", "Summarize provenance", "Debug script", "Clean script"
  )
  options.df <- data.frame(options = options, sep = ":", nchar = c(
    "\"q\"", "\"help\"", "\"ls\"", "\"BO\"", "\"AO\"",
    "\"S\"", "\"DB\"", "\"C\""
  ))
  # Output the help menu to the user
  menu <- capture.output(gdata::write.fwf(options.df, width = c(max(nchar(options)), 1, 6), justify = "l"))[-1]
  cat(menu, sep = "\n")
}

#' @name print.nodes
#' @aliases print.nodes
#' @param prove.json provenance file name
#' @return printed data nodes
print.nodes <- function(prov.json) {

  # Parse .json file
  parsed.json <- prov.parse(prov.json)

  # Print procedure and data nodes
  data.nodes <- get.data.nodes(parsed.json) # Get the data nodes
  return(print(data.nodes[, c("name")]))
}

#' @rdname browser
#' @aliases browser
#' @param prov.json provenance file name
#' @param data.object.name name of provenance data object
#' @param history the provenance sequence
#' @return forward lineage
UI.user.input.AO <- function(prov.json, data.object.name, history) {

  # Output lineage
  provBookR.prov.querying(prov.json, data.object.name, history)

  # Ask the user if they would like to generate a provbook
  message("Generate a provbook for this object history? (Y/N):") # Output message to the user
  user.input <- tolower(readline(prompt = "provBookR> ")) # Collect user input
  if (user.input == "y") {

    # Call provbook generator
    UI.user.input.BG(prov.json, data.object.name)
  } else {
    return(TRUE) # Continue provBookR.browser()
  }
}

#' @rdname browser
#' @aliases browser
#' @param prov.json provenance file name
#' @param data.object.name name of provenance data object
#' @param history the provenance sequence
#' @return backward lineage
UI.user.input.BO <- function(prov.json, data.object.name, history) {

  # Output lineage
  provBookR.prov.querying(prov.json, data.object.name, history)

  # Ask the user if they would like to generate a provbook
  message("Generate a provbook for this object history? (Y/N):") # Output message to the user
  user.input <- tolower(readline(prompt = "provBookR> ")) # Collect user input
  if (user.input == "y") {

    # Call provbook generator
    UI.user.input.BG(prov.json, data.object.name)
  } else {
    return(TRUE)
    # Continue provBookR.browser()
  }
}

#' @rdname browser
#' @aliases browser
#' @param file.name name of provenance file
#' @return summarized provenance
UI.user.input.S <- function(file.name) {

  # Run provSummaizeR
  cat("\n")
  message("Running provSummarizeR...")
  cat("\n")
  prov.summarize.run(file.name)
}

#' @rdname browser
#' @aliases browser
#' @param file.name name of provenance file
#' @return provDebugR browser interface
UI.user.input.DB <- function(file.name) {

  # Run provDebugR
  cat("\n")
  message("Running provDebugR...")
  cat("\n")
  debug.init("prov.json")
  debug.browser()
}

#' @rdname browser
#' @aliases browser
#' @param file.name name of provenance file
#' @param data.object.name name of data object
#' @return cleaned .R script
UI.user.input.C <- function(file.name, data.object.name) {

  # Run RClean
  cat("\n")
  message("Running RClean...")
  cat("\n")
  options(prov.json = readLines(file.name))
  my.code <- clean(data.object.name)
  write.code(my.code, file = paste(data.object.name, ".R", sep = ""))
}

#' @rdname browser
#' @aliases browser
#' @param prov.json provenance file
#' @param data.object.name name of data object
#' @return provbook
UI.user.input.BG <- function(prov.json, data.object.name) {

  # Generate provbook
  message(paste("Making provbook for ", "\"", data.object.name, "\"", " ", "from ", prov.json, "...", sep = ""))
  message("Done!")
  cat(paste("provbook for variable ", "\"", data.object.name, "\"", " is stored in ", getwd(), sep = ""))
}
