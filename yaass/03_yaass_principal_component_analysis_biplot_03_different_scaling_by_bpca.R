setwd("//media//kswada//MyFiles//R//yaass")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yaass
# ------------------------------------------------------------------------------

data("yaass", package = "MPsychoR")

str(yaass)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by bpca package
#   - bpca function has method argument options
#       - "gh": corresponding to column metric preserving (alpha = 1)
#       - "jk": row metric preserving (alpha = 0), plus some additional symmetric ones
# ------------------------------------------------------------------------------

library(bpca)


resbi <- bpca(yaass[, 1:5], scale = FALSE, method = c("gh"))  

summary(resbi)


colvec <- c("cadetblue", "chartreuse4")[unclass(yaass$Group)]



# ----------
# We use a scaling factor (var.factor) of 3
# Note that in biplots, it is not uncommon that the vector coordinates are much larger or smaller than the PC score coordinates.
# In such cases e can multiply the vector coordinates by a scaling factor in order to get a better pitcture.
# This is feasible since only the relative of length of the variable vectors is important.
par(mfrow = c(1,2))
plot(resbi, main = "YAASS Biplot: var.factor = 1", obj.color = colvec, var.factor = 1, obj.cex = 0.8, asp = 1, xlim = c(-2, 2), ylim = c(-2, 2))
plot(resbi, main = "YAASS Biplot: var.factor = 3", obj.color = colvec, var.factor = 3, obj.cex = 0.8, asp = 1, xlim = c(-2, 2), ylim = c(-2, 2))


par(mfrow = c(1,1))
plot(resbi, main = "YAASS Biplot: var.factor = 3", obj.color = colvec, var.factor = 3, obj.cex = 0.8, asp = 1, xlim = c(-2, 2), ylim = c(-2, 2))
legend("topleft", legend = c("high risk psychosis", "healthy controls"), pch = 20, col = unique(colvec))



# -->
# With 2 PCs, we explain 89.4% of the variance in the data.
# The first dimension reflects the behavioral measures and the second dimension the neural measures.
# The length of the vectors is proportional to their standard deviations.

