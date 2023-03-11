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
# Model the sampling station effect
#   - Mean of each stage count, at each station, is multiplied by a log-normal random variable, exp(bi), where bi ~ N(0, sigmab^2)
# ------------------------------------------------------------------------------


form <- eggs ~ offset(off) + lo + la + t +
           I(lo * la) + I(lo ^ 2) + I(la ^ 2) + I(t ^ 2) + I(lo * t) + I(la * t) +
           I(lo ^ 3) + I(la ^ 3) + I(t ^ 3) +
           I(lo * la * t) + I(lo ^ 2 * la) + I(lo * la ^ 2) +
           I(lo ^ 2 * t) + I(la ^ 2 * t) + I(la * t ^ 2) + a + I(a * t) + I(t ^ 2 * a)


b_gpql <- glmmPQL(form, random = list(station = ~1), family = quasi(link = log, variance = "mu"), data = solr)



# ----------
summary(b_gpql)


# alternatively .. but it does not work
# anova(b_gpql, type = "marginal")



# ----------
# dropping terms

b4_gpql <- update(b_gpql, ~. - I(lo * t) - I(lo * la * t) - I(lo * t ^ 2) - I(la * t ^ 2) - I(lo ^ 2 * t))


summary(b4_gpql)



# ------------------------------------------------------------------------------
# plot actual vs. fitted,  residuals vs. fitted
# ------------------------------------------------------------------------------

# Note need to add offset
fv <- exp(fitted(b4_gpql) + solr$off)

resid <- solr$egg - fv

f1 <- sort(fv ^ 0.5)



# ----------
graphics.off()
par(mfrow=c(1,3))

plot(fv ^ 0.5, solr$egg ^ 0.5, pch = ".", cex = 3, col = gray(0.4))
abline(0, 1, lwd = 2)

plot(fv ^ 0.5, resid / fv ^ 0.5, pch = ".", cex = 3, col = gray(0.4))

plot(fv ^ 0.5, resid, pch = ".", cex = 3, col = gray(0.4))

# add 1 s.d. and 2 s.d. reference line
lines(f1, f1);  lines(f1, -f1);  lines(f1, 2 * f1, lty = 2)
lines(f1, -2 * f1, lty = 2)



# ------------------------------------------------------------------------------
# Station effect
# ------------------------------------------------------------------------------

intervals(b4_gpql, which = "var-cov")


# -->
# Station effect is somewhat larger than the variablity in the working residuals



# ------------------------------------------------------------------------------
# Compare residuals
# ------------------------------------------------------------------------------

resid0 <- residuals(b4)


# ----------
lattice::xyplot(la ~ lo | stage, data = solr, cex = resid0)

lattice::xyplot(la ~ lo | stage, data = solr, cex = resid)



# -->
# random effects model is better in residuals


