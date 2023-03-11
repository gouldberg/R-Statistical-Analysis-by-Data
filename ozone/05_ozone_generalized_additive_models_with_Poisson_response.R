setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Generalized Additive Model with Poisson response and scale = -1
#   - the ozone data has a response with relatively small integer values
# ------------------------------------------------------------------------------

# scale = -1: negative values for this parameter indicate that the dispersion should be estimated rather than fixed at one.
# Since we do not truly believe the response is Poisson, it seems wise to allow for overdispersion.
# The default of not specifying scale would fix the dispersion at one.

gammgcv <- mgcv::gam(O3 ~ s(temp) + s(ibh) + s(ibt), family = poisson, scale = -1, data = ozone)

summary(gammgcv)



# --> 
# we see that estimated dispersion is indeed somewhat bigger than one = 1.4585
# ibt is not significant.



# ----------
par(mfrow=c(1,3))
plot(gammgcv, residuals = TRUE, select = 1)
plot(gammgcv, residuals = TRUE, select = 2)
plot(gammgcv, residuals = TRUE, select = 3)


# -->
# we see that the selected transformations are quite similar to those observed previously.

