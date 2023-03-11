setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension



# ------------------------------------------------------------------------------
# plot trajectories of middle (19) and extreme (10, 17, 18, 14)
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2, 2), mar = c(2,2,2,2))

plot(ftensionNP$fdata.est[c(19)], main = "middle: 19", col = c(1))

plot(ftensionNP$fdata.est[c(14,18)], main = "Positive 14, 18 Comp 1", col = c(1:2))

plot(ftensionNP$fdata.est[c(10)], main = "Negatice 10 Comp 1", col = c(1))

plot(ftensionNP$fdata.est[c(17)], main = "positive 17 Comp 2", col = c(1))

