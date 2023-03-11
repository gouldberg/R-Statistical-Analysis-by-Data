setwd("//media//kswada//MyFiles//R//fabric")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# Diagnostics:  Z and Q statistics  (Royston and Wright)
#   - This statistics are useful for testing normality of the residuals within ranges of an independent x-variable
#
#   - The statistics Z(g1), Z(g2), Z(g3), Z(g4) are calcualted from the residuals in group g to test whether the residuals in group g have
#     population mean 0, variance 1, moment-based skewness 0 and moment-based kurtosis 3
#     (the values of a standard normal distribution for the true residuals assuming the model is correct)
#   - Rough guide values of |Z(gj)| > 2:  indicative of significant inadequacies in the model.
#
#   - Q(j) = sum(Z(gj)^2)  (j = 1,2,3,4,  g = 1,2,...)
#
#   - D'Agostino K^2 = Z(g3)^2 + Z(g4)^2 for jointly testing whether the skewness of the residuals is different from 0 and the kurtosis is differenct from 3
#     Note the D'Agostino K-2 statistic should be compared to the upper 5% point of a Chi-squared distribution., i.e. 6.0
# ------------------------------------------------------------------------------

qstats <- Q.stats(mPO2, xvar = fabric$x, n.inter = 4)

print(qstats, digits = 3)


