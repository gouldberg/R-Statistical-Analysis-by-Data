setwd("//media//kswada//MyFiles//R//grip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  grip
# ------------------------------------------------------------------------------

data("grip", package = "gamlss.data")


str(grip)

car::some(grip)



# ------------------------------------------------------------------------------
# ordinary diagnostics
# ------------------------------------------------------------------------------

plot(m0)


plot(m1_s2)


plot(m2_s2)




# ------------------------------------------------------------------------------
# Residual diagnostics:  Z and Q statistics
#   - test normality of the residuals within ranges of an independent x-variable
#   - The statistics Z1, Z2, Z3, Z4 are calculated from the residuals in group g have population mean 0, variance 1, moment-based skewness 0 and moment-based kurtosis 3
#   - Q statistics:  Q(j) = sum(Z(gj)^2)  j = 1,2,3,4
#   - Rough guide values of abs(Z(gj)) > 2 be considered as indicative of significant inadequacies in the model
#     Significant positive (or negative values) for g = 1,2,3 or 4 indicate, respectively, that the redisulas have a higher (lr lower) mean, variance, skewness or kurtosis
#     than the standard normal distribution.
#     --> The model for parameter mu, sigma, nu or tau may need more degrees of freedom to over come this.
# ------------------------------------------------------------------------------


round(Q.stats(m0, xvar = grip$age, n.inter = 20), 3)


round(Q.stats(m1_s2, xvar = grip$age, n.inter = 20), 3)


round(Q.stats(m2_s2, xvar = grip$age, n.inter = 20), 3)



# -->
# m1_s2 is better 



# ------------------------------------------------------------------------------
# Residual diagnostics:  multiple worm plots
# ------------------------------------------------------------------------------

wp(m0, xvar = grip$age, ylim.worm = 1.5, n.inter = 4)


wp(m1_s2, xvar = grip$age, ylim.worm = 1.5, n.inter = 4)


wp(m2_s2, xvar = grip$age, ylim.worm = 1.5, n.inter = 4)



# -->
# m1_s2 is better 
