setwd("//media//kswada//MyFiles//R//ew_eng")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EW_eng
# ------------------------------------------------------------------------------

data(EW_eng, package = "smacof")


str(EW_eng)


EW_eng$east

EW_eng$west



# ------------------------------------------------------------------------------
# Compare MDS plot by Procrustean transformations
#   - When comparing MDS plots, one can eliminate such meaningless differences optimally by Procrustean transformations.
#     If configuration X is taken as the target, the other configuration Y is rotated, reflected, translated, and adjusted in its sixze to optimally match X.
#     All these transformations are similarity transformations that do not change the structure of the MDS configurations.
#     Differences between two configurations that can be eliminated by similarity transformations cannot possibly be meaningful,
#     because they are not caused by the data.
# ------------------------------------------------------------------------------

fit2 <- Procrustes(res.west$conf, res.east$conf)


fit2



# ----------
graphics.off()

par(mfrow=c(1,1))
plot(fit2)



# -->
# Apart from their significant point-to-point similarity, one here notes that both configurations can be partitioned in the same way
# by Alderfer's E(xistence), R(elations), and G(rowth) theory.
# This is a higher-order form of similarity, and it may hold even if the point-wise correspondence is not that high.

