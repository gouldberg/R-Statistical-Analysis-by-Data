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
# Model the sampling station effect by gam()
#   - Mean of each stage count, at each station, is multiplied by a log-normal random variable, exp(bi), where bi ~ N(0, sigmab^2)
# ------------------------------------------------------------------------------

# Function gam() from mgcv package can fit models with simple i.i.d. Gaussian random effects using Laplace approximate restricted or profile likelihood.

# adding terms s(station, bs = "re")

form2 <- eggs ~ offset(off) + lo + la + t +
           I(lo * la) + I(lo ^ 2) + I(la ^ 2) + I(t ^ 2) + I(lo * t) + I(la * t) +
           I(lo ^ 3) + I(la ^ 3) + I(t ^ 3) +
           I(lo * la * t) + I(lo ^ 2 * la) + I(lo * la ^ 2) +
           I(lo ^ 2 * t) + I(la ^ 2 * t) + I(la * t ^ 2) + a + I(a * t) + I(t ^ 2 * a) +
          s(station, bs = "re")
  

b_gam <- gam(form2, family = quasi(link = log, variance = "mu"), data = solr, method = "REML")



# ----------
summary(b_gam)



# ----------
# dropping terms

b4_gam <- update(b_gam, ~. - I(lo * t) - I(lo * la * t) - I(lo * t ^ 2) - I(la * t ^ 2) - I(lo ^ 2 * t))



# ----------
summary(b4_gam)


anova(b4_gam)



# ----------
# variance component estimates
gam.vcomp(b4_gam)



# ------------------------------------------------------------------------------
# Prediction from fitted model and residuals
# ------------------------------------------------------------------------------

# Prediction in which the random effects for station are set to zero. 
# fv_gam <- predict(b4_gam, newdata = solr, exclude = "s(station)")


# Prediction in which the random effects for station are set. 
fv_gam <- predict(b4_gam, newdata = solr)


resid_gam <- solr$egg - fv_gam

f1_gam <- sort(fv_gam ^ 0.5)



# ----------
graphics.off()
par(mfrow=c(2,2))

plot(fv ^ 0.5, solr$egg ^ 0.5, pch = ".", cex = 3, col = gray(0.4))

plot(fv_gam ^ 0.5, solr$egg ^ 0.5, pch = ".", cex = 3, col = gray(0.4))

plot(fv_gam ^ 0.5, resid_gam / fv_gam ^ 0.5, pch = ".", cex = 3, col = gray(0.4))

plot(fv_gam ^ 0.5, resid_gam, pch = ".", cex = 3, col = gray(0.4))

# add 1 s.d. and 2 s.d. reference line
lines(f1_gam, f1_gam);  lines(f1_gam, -f1_gam);  lines(f1_gam, 2 * f1_gam, lty = 2)
lines(f1_gam, -2 * f1_gam, lty = 2)



# ------------------------------------------------------------------------------
# Compare residuals
# ------------------------------------------------------------------------------

lattice::xyplot(la ~ lo | stage, data = solr, cex = resid0)

lattice::xyplot(la ~ lo | stage, data = solr, cex = resid)


# ..?
lattice::xyplot(la ~ lo | stage, data = solr, cex = resid_gam)



