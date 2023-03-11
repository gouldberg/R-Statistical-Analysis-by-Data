setwd("//media//kswada//MyFiles//R//brain")

packages <- c("dplyr", "lattice", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  brain
# ------------------------------------------------------------------------------
data(brain, package = "gamair")

str(brain)



# 2 voxels appear problematic, these voxels have medFPQ values recorded as 3 * 10^-6 and 4 * 10^-7, while the remaining 1565 voxels have values in the range 0.003 to 20.
# Residual plots from all attempts to model the data set including these two voxels consistently show them as grotesque outliers.

# exclude 2 outliers.
brain <- brain[brain$medFPQ > 5e-3,]



# ------------------------------------------------------------------------------
# Simulate and prepare second set of brain scan data
# ------------------------------------------------------------------------------

brain1 <- brain

mu <- fitted(m2)
n <- length(mu)


ind <- brain1$X < 60 & brain1$Y < 20
mu[ind] <- mu[ind] / 3


set.seed(1)
brain1$medFPQ <- rgamma(rep(1, n), mu / m2$sig2, scale = m2$sig2)

brain2 <- rbind(brain, brain1)

brain2$sample1 <- c(rep(1, n), rep(0, n))
brain2$sample0 <- 1 - brain2$sample1



# ------------------------------------------------------------------------------
# In some circumstances it is also interesting to compare completely independent surfaces in a similar way
# This approach is often prefereble to a model with two completely separate surfaces, for reasons of parsimony
# In this example, the data for the two surfaces were available on exactly the same regular mesh, but the approach is equally applicable for
# irregular data where the data are not at the same covariate values for the two surfaces.
# ------------------------------------------------------------------------------

# Fit a model with a single combined surface for both data set
m.same <- gam(medFPQ ~ s(Y, X, k = 100), data = brain2, family = Gamma(link = log))


# 2nd model where the surfaces are allowed to differ
m.diff <- gam(medFPQ ~ s(Y, X, k = 100) + s(Y, X, by = sample1, k = 100), data = brain2, family = Gamma(link = log))



# -----------
summary(m.same)
summary(m.diff)

AIC(m.same, m.diff)


# -->
# Examination of the GCV scores for the two models suggests that the second model m.diff is preferable to m.same.
# AIC is lower for m.diff



# ----------
anova(m.diff)
