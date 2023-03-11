setwd("//media//kswada//MyFiles//R//sole")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sole
# ------------------------------------------------------------------------------

data("sole", package = "gamair")

str(sole)

head(sole)



# ------------------------------------------------------------------------------
# Model space-time smoothers by gamm()
#   - gamm() in mgcv package fits GAMMs based on linear mixed models as implemented in the nlme library.
#   - The function performs the reparameterizations and calls lme to estiamte the re-parameterized model, either directly, or as part of a PQL iteration.
#     But note that changes in the underlying optimzation methods may lead to slight differences between the results obtained with different mgcv and nlme versions.
#   - gamm() gives full access to the random effect and correlation structures available in lme()
# ------------------------------------------------------------------------------


solr$station <- factor(with(solr, paste(-la, -lo, -t, sep = "")))



# ----------
# a tensor product of a thin plate regression spline of lo and la
# a thin plate regression spline (or any other spline) ot t.
# isotropy is a reasonable assumption for spatial dependence
# (although in that case we should really use a more isotropic co-ordinate system than longitude and latitude), but not for the space-time interaction.


som <- gamm(eggs ~ te(lo, la, t, bs = c("tp", "tp"), k = c(25, 5), d = c(2, 1)) +
              s(t, k = 5, by = a) + offset(off),
            family = quasipoisson, data = solr, random = list(station = ~1))


# ----------
som$gam



# ------------------------------------------------------------------------------
# Model space-time smoothers by bam()
#   - somewhat quicker fitting
# ------------------------------------------------------------------------------

som1 <- bam(eggs ~ te(lo, la, t, bs = c("tp", "tp"), k = c(25, 5), d = c(2, 1)) +
              s(t, k = 5, by = a) + offset(off) + s(station, bs = "re"),
            family = quasipoisson, data = solr)


# -->
# fitting now takes one third of the time that gamm takes, and the default is to use REML for the working model estimation.



# ----------
graphics.off()
par(mfrow = c(1,1))
plot(som1)




# ------------------------------------------------------------------------------
# Estimates of the random effect variances
# ------------------------------------------------------------------------------

gam.vcomp(som1)



# ----------
# for gamm model
som$lme


# -->
# The station random effect standard deviation = 0.97 and residual standard deviation = 0.53
# g.0 and g are dummy variables associated with re-casting the smooths as random effects and can be ignored.


