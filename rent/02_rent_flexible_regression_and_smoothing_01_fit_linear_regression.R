setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Fit the linear regression
# ------------------------------------------------------------------------------

r1 <- gamlss(R ~ Fl + A + H + loc, family = NO, data = rent)

l1 <- lm(R~ Fl + A + H + loc, data = rent)



# ----------
summary(l1)

summary(r1)



# -->
# The coefficient estimates of the two fits are identical.



# ----------
# The MLE of sigma  = 308.48 = exp(5.73)
fitted(r1, "sigma")[1]
summary(r1)


# -->
# slight difference from l1 model (308.9)



# ----------
# R^2
Rsq(r1)



# ----------
plot(r1)


# -->
# The distributional assumption of normality is easily rejected by looking at the normal Q-Q plot.
# The residuals are positively skewed.
# The plot of residuals against fitted values indicate variance heterogeneity, in particular that the variance increases with the mean.



# ------------------------------------------------------------------------------
# Plot of the fitted terms
# ------------------------------------------------------------------------------

term.plot(r1, pages = 1, ask = FALSE)

# The shaded areas are the pointwise 95% confidence bands for the smoothing curves and factor levels.
