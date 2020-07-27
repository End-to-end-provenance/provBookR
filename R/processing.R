#' Provenance browser for provBookR
#' @name processing
#' @param parsed.prov.df the parsed data frame provenance
provBookR.prov.processing <- function(parsed.prov.df) {
  provBook.prov.querying(parsed.prov.df)

  # Collect the expr from the ddg.df
  ddg.df.sub.expr <- as.data.frame(ddg.df[, c("expr")], stringsAsFactors = F)

  # Collect the number of pages
  number.pages <- dim(ddg.df.sub.expr)[1]
  number.pages.temp <- number.pages

  # Collect the data from the ddg.df (Make sure these aren't stored in the ddg)
  # Collect the data from the appropriate location
  ddg.df.sub.values <- list()
  while (number.pages.temp != 0) {
    data.read <- paste(prov.dir.test, "/data/", number.pages.temp, "-", "my.data.frame.csv", sep = "")
    ddg.df.sub.values[[number.pages.temp]] <- as.data.frame(read.csv(data.read))
    number.pages.temp <- number.pages.temp - 1
  }

  # Add a row to the data frame to print nothing if the ddg.df.sub is non-even
  if (is.even(number.pages)) {
    # Do nothing
  } else {

    # Add an "empty" string to the expr matrix
    ddg.df.sub.expr <- rbind(ddg.df.sub.expr, " ")

    # Add an "empty" string to the value/data list
    ddg.df.sub.values <- list.prepend(" ", ddg.df.sub.values)
  }

  # Collect the provenance expression and data
  processed.prov.df <- list(ddg.df.sub.expr, ddg.df.sub.values)

  # Pass the processed provenance on
  return(processed.prov.df)
}
