#' Provenance querying for provBookR
#'
#' @name querying the provenance for specific values
#' @param prov.json provenance file
#' @param data.object.name name of the data object file
#' @param history object provenance
#' @importFrom provParseR get.data.nodes
#' @importFrom provParseR get.proc.nodes
#' @importFrom provParseR get.variable.named
#' @importFrom provGraphR create.graph
#' @importFrom provGraphR get.lineage
provBookR.prov.querying <- function(prov.json, data.object.name, history) {

  # Parse the prov.json file:
  parsed.json <- prov.parse(prov.json)
  if (provBookR.check.nodes(parsed.json, data.object.name)) {

    # Get the corresponding node.id
    parsed.prov <- get.variable.named(prov.parse(prov.json), data.object.name) # Query the provenance
    node.id <- parsed.prov[1, c("id")]

    # Collect the variable lineage for the procedure node
    switch(history, "AO" = lineage <- provBookR.lineage(prov.json, node.id, "AO"),
      "BO" = lineage <- provBookR.lineage(prov.json, node.id, "BO")
    )
    variable.history <- provBookR.variable.lineage(prov.json, lineage)
    return(print(variable.history))
  } else {
    message(paste("provenance for", data.object.name, "does not exist", sep = " "))
  }
}

# Checks the existence of data.object.name
# Input: Parsed .json, data object name
# Output: Boolean
provBookR.check.nodes <- function(parsed.json, data.object.name) {

  # Get the data node names
  data.nodes <- get.data.nodes(parsed.json)

  # Check if the data.object.name exist
  if (data.object.name %in% data.nodes[, c("name")]) {
    return(TRUE) # data.object.name is in the provenance
  } else {
    return(FALSE) # data.object.name is not available
  }
}

# Shows data.object.name lineage
# Input: Parsed .json, lineage
# Output: Variable lineage
provBookR.variable.lineage <- function(prov.json, lineage) {

  # Collect the variable lineage
  proc.nodes <- get.proc.nodes(prov.parse(prov.json)) # Collect the procedure or data nodes
  data.nodes <- get.data.nodes(prov.parse(prov.json)) # Collect the procedure or data nodes
  var.vector <- c()
  for (i in 1:length(lineage)) {
    if (grepl("p", lineage[i])) {
      # Collect procedure nodes
      var.vector <- c(proc.nodes[proc.nodes$id == lineage[i], c("name")], var.vector)
    } else {
      # Collect data nodes
      var.vector <- c(data.nodes[data.nodes$id == lineage[i], c("name")], var.vector)
    }
  }
  # Give the user back a lineage of the R object
  return(var.vector)
}

# Collects provenance lineage
# Input: .json file, node.id, direction
# Output: Node lineage
provBookR.lineage <- function(prov.json, node.id, direction) {

  # Create adj.graph
  adj.graph <- create.graph(prov.json, isFile = T)

  # Collect the lineage from the provenance
  switch(direction,
    "AO" = lineage <- get.lineage(adj.graph, node.id, forward = T),
    "BO" = lineage <- get.lineage(adj.graph, node.id, forward = F),
    "default" = message("provenance direction has not been specified.") # Output message to the user
  )

  # Return the lineage from provGraphR
  return(lineage)
}
