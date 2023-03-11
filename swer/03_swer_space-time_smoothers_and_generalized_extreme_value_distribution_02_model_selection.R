setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)




# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------

# Replacing the space time interaction term with additive structure:  s(N, E) + s(year), which decreased the model AIC.
# After that a backward selection approach was used in which anova was used to obtain p-values for each term, and the term with the highest p-value
# (over 0.05) was dropped, provided it also decreased the AIC.
# For this data, the results are rather clear cut, ending up with the following


#b0 <- gam(list(exra ~ s(nao) + s(elevation) + climate.region + te(N, E, year, d = c(2,1), k = c(20, 5)),
#               ~ s(year) + s(nao) + s(elevation) + climate.region + s(N, E),
#               ~ s(elevation) + climate.region),
#          family = gevlss, data = swer)


b <- gam(list(exra ~ s(nao) + s(elevation) + climate.region + s(N, E),
               ~ s(year) + s(elevation) + climate.region + s(N, E),
               ~ climate.region),
          family = gevlss, data = swer)



# ----------
summary(b0)

summary(b)



# ----------
# this resuces AIC
AIC(b, b0)



# ----------
graphics.off()

par(mfrow = c(3,3))

plot(b0, scale = 0, scheme = c(1,1,3,1,1,3), contour.col = "white")


par(mfrow = c(2,2))

plot(b, scale = 0, scheme = c(1,1,3,1,1,3), contour.col = "white")



# -->
# Notice the increase in location and scale in the west of the country, not in the rain shadow of high mountains.

# As expected, elevation generally increases both location and scale,
# although the complex estimate for the dependence on elevation is quite likely to result from confounding with hidden variables,
# such as aspect (e.g., west slope versus east slope).

# NAO index effect is interesting, with the location parameter apparently having a minimum when the index is around zero:
# Switzerland is, of course, a long way inland, so this effect is likely to be quite complex.

# Finally there is a weak year effect evident in the variablity.



# ------------------------------------------------------------------------------
# Plot residuals and raw data against expected value
# ------------------------------------------------------------------------------

mu <- fitted(b)[,1]

rho <- fitted(b)[,2]

xi <- fitted(b)[,3]

fv <- mu + exp(rho) * (gamma(1 - xi) - 1) / xi


graphics.off()
par(mfrow=c(1,3))

plot(resid(b, type = "d") ~ fv, pch = ".", cex = 3, col = gray(0.4), xlab = "fitted values", ylab = "deviance residuals")
plot(resid(b, type = "p") ~ fv, pch = ".", cex = 3, col = gray(0.4), xlab = "fitted values", ylab = "Pearson residuals")
plot(swer$exr ~ fv, pch = ".", cex = 3, col = gray(0.4), xlab = "fitted values", ylab = "Observed response")


