setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ------------------------------------------------------------------------------
# Normal Quantile-quantile Plot:  Studentized residuals vs. Normal Quantiles
# ------------------------------------------------------------------------------

car::qqPlot(rstudent(mod_obj), xlab = "Normal Quantiles", ylab = "Studentized residuals")


# -->
# outliers: 1 and 18



# -->
# For GLMs with discrete responses, such plots are often disappointing, even with a reasonably good-fitting model, because
# (a) possible outlierrs can appear at both the lower and upper ends of the distribution of residuals
# (b) the theoretical normal distribution used to derive the envelope may not be well approximated in a given model


