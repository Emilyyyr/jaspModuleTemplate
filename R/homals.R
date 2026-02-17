homals <- function(jaspResults, dataset, options) {

  # # Returns TRUE if and only if an option has been assigned in the GUI
  # .isAssigned <- function(option) {
  #   not_assigned <- as.character(option) == ""
  #   return(!not_assigned)
  # }

  # homals needs at least 2 variables assigned so it doesnt run with one
  ready <- !is.null(dataset) && ncol(dataset) >= 2 #&& .isAssigned(options$xs)

  if (ready) {
    # do analysis, fill table
    res <- homals::homals(dataset)
  }

  # create empty table, add to jaspResult
  .createExampleTable2(res, jaspResults, dataset, options, ready = ready) # dimensions

  .createExampleTable3(res, jaspResults, dataset, options, ready = ready) # discriminant

  .createAlphaTable(res, jaspResults, dataset, options, ready = ready)

  if (options$discrim_plot) {
    .discrimPlot(res, jaspResults, dataset, options, ready)
  }

  if (options$joint_plot) {
    .jointPlot(res, jaspResults, dataset, options, ready)
  }
}

###### loadings table
.createExampleTable2 <- function(res, jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["myTable"]]))
    return(jaspResults[["myTable"]])

  myTable <- createJaspTable(title = gettext("Loadings"))
  myTable$dependOn("ts")

  jaspResults[["myTable"]] <- myTable

  if (!ready) {
    myTable$addColumnInfo(name = "variable", title = gettext("Variable"), type = "string")
    return()
  }

  #Weights (quantifications) for each option across dimensions.
  res_loadings <- res$loadings

  res_loadings2 <- do.call(rbind, lapply(res_loadings, function(x) x[1, ]))

  myTable$addColumnInfo(name = "variable", title = gettext("Variable"), type = "string")
  for (i in seq_len(ncol(res_loadings2))) {
    myTable$addColumnInfo(name = paste0("dim", i), title = gettextf("Dimension %d", i), type = "number")
  }
  res_loadings3 <- cbind(
    data.frame(variable = rownames(res_loadings2)),
    res_loadings2
  )
  colnames(res_loadings3)[-1] <- paste0("dim", seq_len(ncol(res_loadings2)))
  myTable$setData(res_loadings3)

  return(myTable)

}


###### discrimination table
.createExampleTable3 <- function(res, jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["myTable3"]]))
    return()

  myTable3 <- createJaspTable(title = gettext("Discrimination Indexes"))
  myTable3$dependOn("ts")

  jaspResults[["myTable3"]] <- myTable3

  myTable3$addColumnInfo(name = "variable", title = gettext("Variable"), type = "string")
  if (!ready)
    return()

  res_discrim <- res$discrim
  for (i in seq_len(ncol(res_discrim))) {
    myTable3$addColumnInfo(name = paste0("dim", i), title = gettextf("Dimension %d", i), type = "number")
  }

  # Fill the table
  # Discrimination index for each manifest variable
  res_discrim2 <- cbind(
    data.frame(variable = rownames(res_discrim)),
    res_discrim
  )

  myTable3$setData(res_discrim2)

  return()

}


###### discrimination plot
.discrimPlot <- function(res, jaspResults, dataset, options, ready) {
 if(!is.null(jaspResults[["discrimPlot"]]) && !options$discrim_plot)
   return()

 discrimPlot <- createJaspPlot(title = "Discrimination Plot",  width = 640, height = 640)
 discrimPlot$dependOn(c("variables", "discrim_plot"))

 jaspResults[["discrimPlot"]] <- discrimPlot

 if (!ready)
   return()

 .discrimFillPlot(res, discrimPlot, dataset, options)

 return()
}

.discrimFillPlot <- function(res, discrimPlot, dataset, options) {

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
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = D1, y = D2, label = variable)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_text(vjust = -0.7)+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 1.2),
                                name = 'Dimension 1') +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 1.2),
                                name = 'Dimension 2')

  # add jasp theme
  plot <- plot + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  discrimPlot$plotObject <- plot

  return()
}


###### alpha table
# Keep this as-is (it builds the quantified item matrices per dimension)
.quantifiedItemsByDim <- function(res, dataset) {
  quants <- res$catscores
  if (is.null(quants) || length(quants) == 0)
    return(NULL)

  firstQ <- Filter(Negate(is.null), quants)[[1]]
  if (is.null(firstQ)) return(NULL)

  ndim <- ncol(firstQ)
  n    <- nrow(dataset)
  p    <- length(quants)

  out <- replicate(ndim, matrix(NA_real_, nrow = n, ncol = p), simplify = FALSE)
  varNames <- colnames(dataset)

  for (j in seq_len(p)) {
    qmat <- quants[[j]]
    if (is.null(qmat)) next

    xf <- as.factor(dataset[[j]])
    levels_xf <- levels(xf)
    rn <- rownames(qmat)

    map_idx <- match(levels_xf, rn)

    for (d in seq_len(ndim)) {
      out[[d]][, j] <- qmat[map_idx[as.numeric(xf)], d]
    }
  }

  for (d in seq_len(ndim)) {
    colnames(out[[d]]) <- varNames
  }

  return(out)
}

# NEW: Cronbach's alpha via psych::alpha()
.cronbachAlpha_psych <- function(item_matrix) {
  # psych::alpha expects numeric matrix/data.frame
  X <- as.data.frame(item_matrix)

  # If you prefer listwise deletion (matches your previous na.omit behavior)
  X <- stats::na.omit(X)

  if (ncol(X) < 2L || nrow(X) < 2L)
    return(NA_real_)

  # Avoid messages/prints in JASP
  a <- psych::alpha(X, check.keys = FALSE, warnings = FALSE)

  as.numeric(a$total$raw_alpha)
}

.createAlphaTable <- function(res, jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["alphaTable"]]))
    return()


  alphaTable <- createJaspTable(title = gettext("Reliability (Cronbach's α)"))
  alphaTable$dependOn("ts")

  alphaTable$addColumnInfo(name = "dimension", title = gettext("Dimension"), type = "string")
  alphaTable$addColumnInfo(name = "alpha",     title = gettext("Cronbach's α"), type = "number")
  alphaTable$addColumnInfo(name = "k_items",   title = gettext("Items (k)"), type = "integer")

  jaspResults[["alphaTable"]] <- alphaTable

  if (!ready || is.null(res))
    return()

  X_by_dim <- .quantifiedItemsByDim(res, dataset)
  if (is.null(X_by_dim)) {
    return()
  }

  ndim <- length(X_by_dim)
  alphas <- numeric(ndim)
  ks     <- integer(ndim)

  for (d in seq_len(ndim)) {
    Xd <- X_by_dim[[d]]
    ks[d] <- ncol(Xd)
    alphas[d] <- .cronbachAlpha_psych(Xd)
  }

  alpha_df <- data.frame(
    dimension = paste0("Dimension ", seq_len(ndim)),
    alpha     = alphas,
    k_items   = ks,
    stringsAsFactors = FALSE
  )

  alphaTable$setData(alpha_df)
  return()
}


###### joint plot

.jointPlot <- function(res, jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["jointPlot"]]) && !options$joint_plot)
    return()

  jointPlot <- createJaspPlot(title = "Joint Plot",  width = 640, height = 640)
  jointPlot$dependOn(c("variables", "joint_plot"))

  jaspResults[["jointPlot"]] <- jointPlot

  if (!ready)
    return()

  .jointFillPlot(res, jointPlot, dataset, options)

  return()
}


.jointFillPlot <- function(res, jointPlot, dataset, options) {

  cats <- res$catscores
  if (is.null(cats) || length(cats) == 0)
    stop("Joint plot requires category scores (res$catscores).")

  # Need at least 2 dimensions
  firstQ <- Filter(Negate(is.null), cats)[[1]]
  if (is.null(firstQ) || ncol(firstQ) < 2)
    stop("Joint plot requires at least 2 dimensions (ndim >= 2).")

  # ---- build long data frame of category points ----
  cat_df <- do.call(rbind, lapply(names(cats), function(v) {
    q <- cats[[v]]
    if (is.null(q)) return(NULL)

    data.frame(
      type     = "Category",
      variable = v,
      label    = paste(v, rownames(q), sep = ": "),
      D1       = as.numeric(q[, 1]),
      D2       = as.numeric(q[, 2]),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(cat_df) || nrow(cat_df) == 0)
    stop("No category points available for joint plot.")

  # ---- optionally overlay object scores if available ----
  obj_df <- NULL
  # common field names in homals objects
  for (nm in c("objscores", "objectscores", "object.scores", "scores", "xscores")) {
    if (!is.null(res[[nm]])) {
      os <- res[[nm]]
      if (is.matrix(os) || is.data.frame(os)) {
        os <- as.matrix(os)
        if (ncol(os) >= 2) {
          obj_df <- data.frame(
            type     = "Object",
            variable = NA_character_,
            label    = NA_character_,
            D1       = as.numeric(os[, 1]),
            D2       = as.numeric(os[, 2]),
            stringsAsFactors = FALSE
          )
        }
      }
      break
    }
  }

  # ---- ggplot joint plot ----
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.6) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, alpha = 0.6) +

    # category points + labels
    ggplot2::geom_point(
      data = cat_df,
      ggplot2::aes(x = D1, y = D2),
      size = 2
    ) +
    ggplot2::geom_text(
      data = cat_df,
      ggplot2::aes(x = D1, y = D2, label = label),
      vjust = -0.7,
      size = 3
    ) +

    # optional object scores (no labels)
    { if (!is.null(obj_df)) ggplot2::geom_point(
      data = obj_df,
      ggplot2::aes(x = D1, y = D2),
      alpha = 0.35,
      size = 1.3
    ) else NULL } +

    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = 1.2),
      name = "Dimension 1"
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = 1.2),
      name = "Dimension 2"
    ) +
    ggplot2::coord_equal() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  jointPlot$plotObject <- p
  return()
}



