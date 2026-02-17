data(galo)
##simple homals 2D-homals solution (School not active)
res <- homals(galo, active = c(rep(TRUE, 4), FALSE))
res

##predict IQ
res <- homals(galo, active = c(rep(TRUE, 4), FALSE), sets = list(c(1,3,4),2,5))

##Single ranks for each variable (non-linear PCA)
res <- homals(galo, active = c(rep(TRUE, 4), FALSE), sets = list(c(1,3,4),2,5))

##Nonlinear canonical correlation (2 sets of variables)
res <- homals(galo, active = c(rep(TRUE, 4), FALSE), sets = list(c(1,2),c(3,4),5))

##regression (linear)
data(neumann)
res <- homals(neumann, sets = list(3, 1:2), level = "numerical", rank = 1)

##regression (monotone)
res <- homals(neumann, sets = list(3, 1:2), level = "ordinal", rank = 1)

##3-dimensional senate solution
data(senate)
res <- homals(senate, active = c(FALSE, rep(TRUE, 20)), ndim = 3)

##Predicting Species in iris dataset (polynomial level constraints for predictors)
data(iris)
res <- homals(iris, sets = list(1:4,5), level = c(rep("polynomial",4),"nominal"),
              rank = 2, itermax = 1000)





install.packages("rgl")
install.packages("Gifi")

library(rgl)
library("homals")
library("Gifi")
data("senate")

res <- homals(senate, active = c(FALSE, rep(TRUE, 20)), ndim = 3)
plot3d(res, plot.type = "objplot", sphere = FALSE, bgpng = NULL)
+ xlim=c(-2,3),ylim=c(-2,3), asp=1)

plot(res,plot.type="spanplot",plot.dim=c(1,3),var.subset=1,
+ xlim=c(-2,3),ylim=c(-2,3),asp=1)

plot(res,plot.type="spanplot",plot.dim=c(2,3),var.subset=1,
+ xlim=c(-2,3),ylim=c(-2,3),asp=1)
plot(res, plot.type = "spanplot", plot.dim = c(1, 2), var.subset = 1,


debugonce(homals)
res <- homals(galo, active = c(rep(TRUE, 4), FALSE))
res
res$loadings





















# make res
res <- homals(galo, active = c(rep(TRUE, 4), FALSE))
res


#Weights (quantifications) for each option across dimensions.
res$loadings


#Discrimination index for each manifest variable.
res$discrim
plot(res, plot.type="dmplot")


#Alpha coefficient for each dimension.
#install.packages("psych")
library(psych)

homals_alpha_by_dim <- function(res) {
  if (is.null(res$scoremat)) {
    stop("res$scoremat is missing. (Your homals object should contain it.)")
  }

  # scoremat is an array based on category scores. We need it in shape:
  #   n (objects) x p (dimensions) x m (variables)
  sm <- res$scoremat
  d <- dim(sm)

  # Try to detect which axis is "ndim" by matching to length(res$eigenvalues)
  ndim <- length(res$eigenvalues)
  dim_axis <- which(d == ndim)
  if (length(dim_axis) != 1) {
    stop("Could not uniquely identify the dimension axis in scoremat.")
  }

  # Reorder to n x ndim x m
  # If sm is already n x ndim x m -> dim_axis = 2, do nothing
  # If sm is ndim x n x m -> dim_axis = 1
  # If sm is n x m x ndim -> dim_axis = 3
  if (dim_axis == 1) sm <- aperm(sm, c(2, 1, 3))
  if (dim_axis == 3) sm <- aperm(sm, c(1, 3, 2))

  # Now: sm[ i, dim, var ]
  active <- res$active
  if (length(active) == 1 && isTRUE(active)) active <- rep(TRUE, dim(sm)[3])

  # Compute alpha per dimension
  alphas <- sapply(seq_len(dim(sm)[2]), function(k) {
    Xk <- sm[, k, active, drop = FALSE]  # n x m_active
    Xk <- as.data.frame(Xk)
    suppressWarnings(psych::alpha(Xk)$total$raw_alpha)
  })

  names(alphas) <- paste0("D", seq_along(alphas))
  alphas
}

# Example usage:
# res <- homals(galo, active = c(rep(TRUE, 4), FALSE), ndim = 2)
alpha_dim <- homals_alpha_by_dim(res)
alpha_dim


#Recording capabilities for both scores and quantifications.

#Graphical representation of the first two latent dimensions.


#Ability to extract multiple dimensions or latent variables.























# Analysis
f <- function(x) { options$a * x^2 } # Function to be plotted
p <- ggplot2::ggplot() +             # Plotting command
  ggplot2::xlim(-3, 3) +
  ggplot2::ylim(0, 10) +
  ggplot2::geom_function(fun = f)
# add jasp theme
p <- p + jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw()
# Aesthetics
parabolaPlot <- createJaspPlot(title = gettext("Discrimination Measures"),
                               width = 160,
                               height = 320)
parabolaPlot$dependOn(c("ts")) # Refresh view whenever a changes
parabolaPlot$info <- gettext("This figure displays the discrimination index on different dimensions for each variable.")
jaspResults[["discrimPlot"]] <- discrimPlot
discrimPlot$plotObject <- p

return()



library(ggplot2)

# Get discrimination values
if (!is.null(res$discrim)) {
  discr <- res$discrim
} else {
  discr <- res$loadings^2
}

# Convert to data frame
df <- data.frame(
  variable = rownames(discr),
  D1 = discr[, 1],
  D2 = discr[, 2]
)

# ggplot version
ggplot(df, aes(x = D1, y = D2, label = variable)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7) +
  labs(
    x = "Dimension 1",
    y = "Dimension 2",
    title = "Discrimination Measures"
  ) +
  theme_minimal()
