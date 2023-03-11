setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# Model by time dependent covariates by gam with Poisson distribution
# ------------------------------------------------------------------------------

# It is quite time consuming, because of the data set size and number of levels of the tf factor...
mod_gam <- gam(z ~ tf - 1 + trt + s(sqrt(protime)) + s(platelet) + s(age) + s(bili) + s(albumin) + s(sqrt(ast)),
         family = poisson, data= pb, method = "REML")


summary(mod_gam)



# ------------------------------------------------------------------------------
# Model by time dependent covariates by bam with Poisson distribution
# ------------------------------------------------------------------------------
# Here let's tolerate some approximation and fit using bam with the discrete == TRUE option (which would still be feasible for millions of data)

# Starting with the dame dependence on covariates as in the baseline covariates case,
# backwards model selection drops alk.phos, sex and stage in that order,
# on the basis of each sequentially being the term with the highest p-value.
# #ach reduction of the model also reduced the AIC.
# As the trial variable of interest, trt was not considered as a candidate for dropping.

# Fitting selected model

# tf - 1 is the first term in the linear predictor to ensure that we get one coefficient estimated for each event time,
# rather than an interceopt and difference terms.
# The option nthreads = 2 tessl bam to use two CPU cores when possible
mod_bam <- bam(z ~ tf - 1 + trt + s(sqrt(protime)) + s(platelet) + s(age) + s(bili) + s(albumin) + s(sqrt(ast)),
         family = poisson, data= pb, discrete = TRUE, nthreads = 2)


summary(mod_bam)


anova(mod_bam)



# ------------------------------------------------------------------------------
# Cumulative hazard by subject, Martingale residuals and deviance
# ------------------------------------------------------------------------------

# cumulative hazard by subject
cahz <- tapply(fitted(mod_bam), pb$id, sum)



# censoring indicaator
d <- tapply(pb$z, pb$id, sum)



# Martingale residuals
mrsd <- d - chaz



# deviance
drsd <- sign(mrsd) * sqrt(-2 * (mrsd + d * log(chaz)))




# ------------------------------------------------------------------------------
# Plot smooth effects
# ------------------------------------------------------------------------------

plot(mod_bam, pages = 1, scale = 0, scheme = 1)


# -->
# Compared to the baseline covariates model there is slighly more non-linearity evident in the estimated effects now,
# although the estimated effect of age is a simple straight line.

