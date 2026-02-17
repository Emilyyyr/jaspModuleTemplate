parabola <- function(jaspResults, dataset, options) {

  # Analysis
  res <- homals::homals(dataset)

  discr <- res$discrim
  if (is.null(discr))
    discr <- res$loadings^2

  # Need at least 2 dimensions for a dmplot
  if (ncol(discr) < 2)
    stop("Discrimination plot requires at least 2 dimensions (ndim >= 2).")

  df <- data.frame(
    variable = rownames(discr),
    D1 = discr[, 1],
    D2 = discr[, 2],
    stringsAsFactors = FALSE
  )

  # ---- ggplot ----
  p <- ggplot2::ggplot(df, ggplot2::aes(x = D1, y = D2, label = variable)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_text(vjust = -0.7)

  # add jasp theme
  p <- p + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  # Aesthetics + JASP plot object
  discrimPlot <- createJaspPlot(
    title  = gettext("Discrimination Measures"),
    width  = 160,
    height = 320
  )
  discrimPlot$dependOn(c("ts"))  # <-- replace "ts" with whatever options actually trigger recomputation
  discrimPlot$info <- gettext("This figure displays the discrimination index on different dimensions for each variable.")

  jaspResults[["discrimPlot"]] <- discrimPlot
  discrimPlot$plotObject <- p

  return()
}
