setwd("//media//kswada//MyFiles//R//chicago")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  chicago
# ------------------------------------------------------------------------------
data(chicago, package = "gamair")

str(chicago)



# ------------------------------------------------------------------------------
# Distributed Lag Model for pollution related deaths
#   - the response depends on a sum of smooth functions of lagged covariates.
#     Usually the smooth functions at different lags vary smoothly with lags.
#     We do not expect the response to yesterday's pollution to have a completely different shape to the reseponse to the day before yesterday's pollution.
# ------------------------------------------------------------------------------

# distributed lag model can be specified uisng a model formula componente te(PM10, lag)
# distributed lag model fits around 60 times faster than the single index model.

apl <- bam(death ~ s(time, bs = "cr", k = 200) + te(pm10, lag, k = c(10, 5)) + te(o3, tmp, lag, k = c(8, 8, 5)), family = poisson, data = dat)



# ----------
# including lags for so2median
dat$so2 <- lagard(chicago$so2median)
apl2 <- bam(death ~ s(time, bs = "cr", k = 200) + te(pm10, lag, k = c(10, 5)) + te(o3, tmp, lag, k = c(8, 8, 5)) + te(so2, tmp, lag, k = c(8, 8, 5)), family = poisson, data = dat)




# ----------
summary(apl)
summary(apl2)


# -->
# Note that the model with and without so2 are estimated with different numbers of data, because of missing so2 data,
# so comparing them by AIC or GLRT (anova) is not valid.
# Additional distributed lag term for so2 does not improve Deviance Explained very much.
# So there seems little justification for including it.



# ----------
gam.check(apl)


# -->
# The residual plot shows one rather large positive outlier, but is otherwise reasonable.



# ----------
par(mfrow=c(1,1))
plot(apl)



AIC(apsi, apl)



# -->
# This model does not do quite as well on the very highest daily death count as the single index model,
# but does not have the one quite large negative outlier evident from that model.
# The distributed lag model has the lowe AIC of the two.



# ------------------------------------------------------------------------------
# Perspective plots of the ozone-temperature interaction by lag
# ------------------------------------------------------------------------------

par(mfrow=c(1,3))
vis.gam(apl, plot.type = "persp", view = c("o3", "tmp"), theta = 30, phi = 30, color = "gray", cond=list(lag = 1))
vis.gam(apl, plot.type = "persp", view = c("o3", "tmp"), theta = 30, phi = 30, color = "gray", cond=list(lag = 3))
vis.gam(apl, plot.type = "persp", view = c("o3", "tmp"), theta = 30, phi = 30, color = "gray", cond=list(lag = 5))



# -->
# The dominant feature is high risk at high ozone high temperature combinations for the middle lags


