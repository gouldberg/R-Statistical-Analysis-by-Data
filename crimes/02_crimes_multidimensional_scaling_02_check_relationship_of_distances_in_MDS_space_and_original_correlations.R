setwd("//media//kswada//MyFiles//R//crimes")

packages <- c("dplyr", "smacof", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crimes
# ------------------------------------------------------------------------------
data("crimes", package = "smacof")

str(crimes)

car::some(crimes)



# ------------------------------------------------------------------------------
# Multidimensional Scaling:
# Check the relationship of distances in MDS space and original correlations
# ------------------------------------------------------------------------------

dat0 <- as.matrix(crimes)

dat <- dat0[ lower.tri(dat0, diag=FALSE) ]



# ----------
dist0 <- as.matrix(result$confdist)

dist <- dist0[lower.tri(dist0, diag=FALSE)]


# ----------
plot( dat, dist, type="p", pch=20, xlab="Correlations", ylab="Distances in MDS space" )



# ----------
fit.line <- lm(dist~dat)
abline(fit.line, col="red")

cor(dist, dat)


# -->
# But this distance correlate with only r = -0.868



# ----------
# Or Shepard Diagram
plot(result, plot.type = "Shepard")

