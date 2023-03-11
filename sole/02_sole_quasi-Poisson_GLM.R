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
# Quasi-poisson GLM
#   - A reasonable starting model might represent log(S) as a cubic function of longitude, latitude and time
#     Mortality might be modelled by a simpler function, say a quadratic in t.
#
#     d(a, X) = S(X) * exp(-delta(x) * a)
#     d(a, X):  density of eggs (per day per square metre of sea surface)
#     S(X):  local spawning rate
#     delta(X):  per capita mortailty rate
#     exp(-delta(x) * a):  local survival rate
#     a:  age
#
#   - The eggs are sampled by hauling a net vertically through the water and counting the number of eggs caught in it.
#     This might suggest a Poisson model, but most such data display overdispersion relative to Poisson, and additionally, 
#     the data are not available as raw counts but rather as densities per m^2 sea surface.
#     These considerations suggest using quasi-likelihood, with the variance proportional to the mean.
# ------------------------------------------------------------------------------

# model offset term
sole$off <- log(sole$a.1 - sole$a.0)



# mean stage age
sole$a <- ( sole$a.1 + sole$a.0 ) / 2



# ----------
# Since polynomial models can lead to numerical stability problems if not handled carefully,
# the covariates are all translated and scaled before fitting.
solr <- sole

solr$t <- solr$t - mean(sole$t)
solr$t <- solr$t / var(sole$t) ^ 0.5
solr$la <- solr$la - mean(sole$la)
solr$lo <- solr$lo - mena(sole$lo)



# ----------
b <- glm(eggs ~ offset(off) + lo + la + t +
           I(lo * la) + I(lo ^ 2) + I(la ^ 2) + I(t ^ 2) + I(lo * t) + I(la * t) +
           I(lo ^ 3) + I(la ^ 3) + I(t ^ 3) +
           I(lo * la * t) + I(lo ^ 2 * la) + I(lo * la ^ 2) +
           I(lo ^ 2 * t) + I(la ^ 2 * t) + I(la * t ^ 2) + a + I(a * t) + I(t ^ 2 * a),
         family = quasi(link = log, variance = "mu"), data = solr)


summary(b)


# -->
# suggesting that doropping the lo * t term and other t related interactions



# ----------
b1 <- update(b, ~. - I(lo * t))

b4 <- update(b, ~. - I(lo * t) - I(lo * la * t) - I(lo * t ^ 2) - I(lo ^ 2 * t))


summary(b1)

summary(b4)



# ----------
anova(b, b4, test = "F")


# -->
# suggesting no reason not to accept the simplified model (b4) ...



# ----------
# The default residual plots are unhelpful, since the large number of zeroes in the data, corresponding to areas where there really are no eggs.
plot(b4)


graphics.off()
par(mfrow = c(1,2))
plot(fitted(b4) ^ 0.5, solr$eggs ^ 0.5, pch = ".", cex = 3, col = gray(0.5))
plot(fitted(b4) ^ 0.5, residuals(b4), pch = ".", cex = 3, col = gray(0.5))



