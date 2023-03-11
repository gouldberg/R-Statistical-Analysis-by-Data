setwd("//media//kswada//MyFiles//R//minima_maxima")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  minima and maxima
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Fit predetermined distribution
# ------------------------------------------------------------------------------

# According to theory, the appropriate asymptotic distribution for the minima values is Gumbel GU, for the maxima the reverse Gumbel RG,
# and for the medians the normal NO

# histDist() uses gamlssMS() as a default algorithm to fit the model, but in addition displays the histogram together with the fitted distribution of the data

par(mfrow = c(1,3))

m1 <- histDist(Ymin, family = GU)

m2 <- histDist(Ymax, family = RG)

m3 <- histDist(Ymid, family = NO)



# ----------
m1

m2

m3
