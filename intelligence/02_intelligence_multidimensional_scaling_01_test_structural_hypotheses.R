setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

labl <- paste0(intelligence$Language, intelligence$Requirement)


idiss <- sim2diss(intelligence[,paste0("T", 1:8)])


# defulat is ratio MDS
fit <- mds(idiss)

fit$conf[,2] <- -fit$conf[,2]



# ----------
graphics.off()

plot(fit)
text(fit$conf[,1] - 0.1, fit$conf[,2], labl, cex = 0.7)
segments(x0 = -2, y0 = -1.3, x1 = 0.5, y1 = 0.8, lty = 2)
segments(x0 = -0.4, y0 = 0.7, x1 = 1.3, y1 = -0.7, lty = 2)



# -->
# We find that the place can indeed be partitioned by a straight line such that 
# all points labeled as "G" are on one side, and all "N" points on the other.
# Also, an A- and an I-region.

