setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# Extract residuals from lme model
# ------------------------------------------------------------------------------

# Conditional data-BLUP
res <- with(hodges, prind) - fitted(m2)

plot(resid(l2, level = 1) ~ res)



# ----------
# Marginal data-fitted
res <- with(hodges, prind) - fitted(m0)

plot(resid(l2, level = 0) ~ res)



# ------------------------------------------------------------------------------
# Extract conditional residuals
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,2), mar = c(2,2,2,2))

# gamlss() residuals are the normalized quantile residuals, which for a normal distribution with constant variance sigma^2 are the standardized residuals
plot(resid(m3))

# gamlss() models have normal distribution with constant variancve var(mu-hat) = 1, so the Pearson's residuals are y - y-hat ...?
res <- with(hodges, prind) - fitted(m3)
plot(res)

# lme() residuals
plot(resid(l2, level = 1))

# lme() residuals also have the option to produce the Pearson residuals = ( y - y-hat ) / sqrt(var(mu-hat))
plot(resid(l2, level = 1, type = "p"))




# ----------
# Pearson residuals from lme model
graphics.off()
par(mfrow=c(1,2), mar = c(2,2,2,2))

r1 <- qNO(pNO(with(hodges, prind), mu = fitted(l2, level = 1), sigma = l2$sigma))

plot(resid(l2, level = 1, type = "p") ~ r1)

# gamlss() residuals is almost person residuals
plot(resid(m3) ~ r1)



# ------------------------------------------------------------------------------
# Extract marginal residuals
# ------------------------------------------------------------------------------
# Note that gamlss() does not provide marginal residulas (but gamlssNP() does)
graphics.off()
par(mfrow=c(1,1))

r0 <- qNO(pNO(with(hodges, prind), mu = fitted(l2, level = 0), sigma = l2$sigma))

plot(resid(l2, level = 0, type = "p") ~ r0)



