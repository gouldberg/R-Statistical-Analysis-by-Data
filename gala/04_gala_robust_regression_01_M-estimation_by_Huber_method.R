]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ------------------------------------------------------------------------------
# models
# ------------------------------------------------------------------------------

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)

lmod_final <- lm(Species ~ Elevation + Adjacent, data = gala)




# ------------------------------------------------------------------------------
# M-estimation by Huber method
#   - weight(residual) =  1 if abs(residual) <= c,  otherwise c / abs(residual)
#   - downweight extreme cases and equal weight for the middle cases
# ------------------------------------------------------------------------------

library(MASS)


rlmod <- rlm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


summary(rlmod)

summary(lmod)


coef(rlmod)

coef(lmod)




# -->
# R2 statistics is not given because it does not make sense in the context of a robust regression
# p-values are not given, too.
# but t-value for Elevation and Adjacent is large, those might be significant
# The numerical values of the coefficients have changed somewhat and the standard errors are generally smaller.

# Although the robust fit gives numerically different output,
# the overall impression of what predictors are significant in explaining the response is unchanged.
# Thus the robust regression has provided some measure of confirmation.



# ------------------------------------------------------------------------------
# weights assigned by the model 
# ------------------------------------------------------------------------------

wts <- rlmod$w

names(wts) <- row.names(gala)


sort(wts)



# -->
# We can ses that a few islands are substantially discounted in the calculation of the robust fit.



