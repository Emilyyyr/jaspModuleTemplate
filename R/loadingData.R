processTable <- function(jaspResults, dataset, options) {

  # Auxiliary function.
  # Returns TRUE if and only if an option has been assigned in the GUI
  .isAssigned <- function(option) {
    not_assigned <- as.character(option) == ""
    return(!not_assigned)
  }

  .createExampleTable(jaspResults, dataset, options,
                      ready = .isAssigned(options$xs))

  # Only if everything has been assigned ...
  if(.isAssigned(options$ts) && .isAssigned(options$xs)) {
    # ... print the inputs as a table
    stats <- createJaspTable(gettext("Some descriptives"))
    stats$dependOn(c("ts", "xs")) # Declare dependencies to make the object disappear / reappear when needed

    stats$addColumnInfo(name = gettext("times"))
    stats$addColumnInfo(name = gettext("xs"))

    stats[["times"]] <- dataset[[options$ts]]
    stats[["xs"]] <- dataset[[options$xs]]

    jaspResults[["stats"]] <- stats
  }

 else {
  expl <- createJaspHtml(text = "Select times and positions")
  expl$dependOn(c("ts", "xs")) # Declare dependencies to make the object disappear / reappear when needed

  jaspResults[["Explanation"]] <- expl
 }
}
.createExampleTable <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["myTable"]]))
    return()
  myTable <- createJaspTable(title = gettext("Mean of my Results"))
  myTable$dependOn("xs")

  myTable$addColumnInfo(name = "mean", title = gettext("Mean of xs"), type = "number")

  jaspResults[["myTable"]] <- myTable

  if (!ready)  # if not all input is provided that is needed to do the computations, return an empty table
    return()

  myTable$addRows(list(mean=mean(dataset[[options$xs]])))
  return()

}
