setwd("//media//kswada//MyFiles//R//meta")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meta
# ------------------------------------------------------------------------------
data("meta", package = "gamlss.data")


str(meta)

car::some(meta)



# ------------------------------------------------------------------------------
# Binomial response with nonparametric random intercept and slope
# ------------------------------------------------------------------------------

library(gamlss.mx)


# mixture = "np":  for nonparametric random effect models
# K:  number of points of the nonparametric mixture if miture = "np"

me1 <- gamlssNP(cbind(d, n - d) ~ fac, random = ~fac | study, K = 10, mixture = "np", family = BI, data = meta, tol = 0.1)


me1


summary(me1)



# ----------
# mass points and associated probabilities

me1$mass.points

me1$prob



# ------------------------------------------------------------------------------
# Plot joint nonparametric distribution (gamma0, gammma1) and the marginal nonparametric distributions
# ------------------------------------------------------------------------------

# The fitted model is:  Y(ij) ~ BI(n(ij), mu(ij))   i = 1,2;  j = 1,2,...,27
# logit(mu(ij)) = gamma(0j) + gamma(1j) (if i = 2)


par(mfrow = c(2,2))

plotMP(me1$mass.points[,1], me1$mass.points[,2], me1$prob, theta = 0, phi = 20, main = "(a)")
plot(me1$mass.points[,1], me1$prob, type = "h", main = "(b)")
points(me1$mass.points[,1], me1$prob)

plotMP(me1$mass.points[,1], me1$mass.points[,2], me1$prob, theta = 90, phi = 20, main = "(c)")
plot(me1$mass.points[,2], me1$prob, type = "h", main = "(d)")
points(me1$mass.points[,2], me1$prob)



# ------------------------------------------------------------------------------
# Means, standard deviations, and correlation of the fitted distribution of (gamma0, gamma1)
# ------------------------------------------------------------------------------

vM <- cov.wt(me1$mass.points, me1$prob)


vM$cov



# ----------
# fitted means
# The overall estimate of the treatment effect (on the logit scale) is 0.5979
vM$center


vM$n.obs

vM$wt



# ----------
# The correlation matrix

cov2cor(vM$cov)



# standard errors

sqrt(diag(vM$cov))



