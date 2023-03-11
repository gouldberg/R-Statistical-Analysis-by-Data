setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practice: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)



# ----------
# Seed was "ordered factor" --> changed to non-ordered factor
Loblolly$Seed <- as.factor(as.character(Loblolly$Seed))


# Without centering, polynomial terms can become highly correlated which can cause numerical difficulties
Loblolly$age <- Loblolly$age - mean(Loblolly$age)




# ------------------------------------------------------------------------------
# Generalized likelihood ratio test and AIC to compare models
# ------------------------------------------------------------------------------

# anova command here is actually conducting a generalized likelihood ratio test.

anova(m3, m2)



# -->
# m3:  dropping corAR1

# Rejects m3 in favor of m2.
# Note that in this case the GLRT assumptions are met: m3 is effectively setting the autocorrelation parameter phi to zero,
# which is in the middle of its possible range, not on a boundary.
# Also reports the AIC for the models, also suggesting that m2 is preferable.



# ----------
anova(m4, m2)



# -->
# m4:  dropping random effect of the dependence of tree-specific growth on the cube of age

# Recall that the GLRT test is somewhat problematic here, since m4 is m2 with some variance parameters set to the edge of the feasible parameter space;
# However, a likelihood ratio statistic so large that it would have given rise to a p-value of 0.0002,
# for a standard GLRT, is marginally strong grounds for rejecting m4 in favor of m2 in the current case.
# Comparison of AIC scores suggests quite emphatically that m2 is the better model.



# ----------
anova(m2, m5)



# -->
# m5:  less general random effects structure:  b(j) ~ N(0, phi), phi is a diagonal matrix (with positive diagonal elements)

# Again both the GLRT test and AIC comparison favour the more general model m2.
# In this case the GLRT assumptions are met: m5 amounts to setting the random effects covariances in m2 to zero,
# but since covariances can be positive or negative this is not on the boundary of the parameter space and the GLRT assumptions hold.
