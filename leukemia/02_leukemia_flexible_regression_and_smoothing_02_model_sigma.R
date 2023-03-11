setwd("//media//kswada//MyFiles//R//leukemia")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Leukemia
# ------------------------------------------------------------------------------

data("Leukemia", package = "gamlss.data")


str(Leukemia)

car::some(Leukemia)



# ------------------------------------------------------------------------------
# Refine model g2 with smoothing for age in sigma
# ------------------------------------------------------------------------------

# Fitting a smooth curve for age in sigma, i.e.

g21 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~ pb(age), data = Leukemia, trace = FALSE)



# -->
GAIC(g1, g2, g3, g4, g5, g6, g21, k = log(nrow(Leukemia)))


plot(g21)


wp(g2)
wp(g21)



# -->
# improves the fit according to the GAIC but not the residuals.


