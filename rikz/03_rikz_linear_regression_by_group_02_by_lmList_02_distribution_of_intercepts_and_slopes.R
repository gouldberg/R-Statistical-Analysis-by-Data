# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# the distribution of intercepts and slopes
# ------------------------------------------------------------------------------

par(mfrow = c(1, 1))

plot(intercepts, slopes, xlab = "Intercept", ylab = "Slope")



# ----------
formula <- ~ slopes + intercepts

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# -->
# Some relationship between intercepts and slopes

