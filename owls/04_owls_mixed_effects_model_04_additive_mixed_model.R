# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)




# ------------------------------------------------------------------------------
# Fit additive mixed model
# ------------------------------------------------------------------------------

library(mgcv)


# No family argument is specified, the gamm function uses the Gaussian distribution.
M6 <- gamm(LogNeg ~ FoodTreatment + s(ArrivalTime), random = list(Nest = ~1), data = Owls)



# The output from gamm is slightly confusing
# The object M6 has an lme component and a gam component
summary(M6)



# ----------
# detailed output on the smoothers and parameteric terms in the models
# The estimated regression parameter for food treatment is the similar to the one obtained by the linear mixed effects model
# The smoother is significant and has nearly 7 degrees of freedom !!
summary(M6$gam)



# ----------
# more compact presentation of the results:  the anova table is NOT doing sequential testing !!
anova(M6$gam)



# ----------
# plot the smoothers --> bad news for the linear mixed effects model: the effect of arrival time is non-linear.
# it seems that there is a lot of sibling negotiation at around 23.00 hours and a second (through smaller) peak at about 01.00 - 02.00 hours.
plot(M6$gam)



# ----------
# plots the normalized residuals versus fitted values and can assess homogeneity
plot(M6$lme)



# ----------
# Detailed output on the estimated variances
summary(M6$lme)

