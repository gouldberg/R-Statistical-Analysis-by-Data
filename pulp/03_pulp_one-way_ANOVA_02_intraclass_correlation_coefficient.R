setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# ANOVA estimators and
# intraclass correlation coefficient
# ------------------------------------------------------------------------------

# For random effects model, 
# we can compute the variance of the operator effects

# number of samples for each operator (this is balanced data)
n <- 5
# number of levels
a <- 4

summary(lmod)



# ----------
SSE <- 1.70
SSA <- 1.34

( MSE <- SSE / (a * (n - 1)) )
( MSA <- SSA / (a - 1) )

( sigma2_alpha <- (MSA - MSE) / n )



# ----------
# intraclass correlation coefficient
sigma2_alpha / (sigma2_alpha + MSE)




