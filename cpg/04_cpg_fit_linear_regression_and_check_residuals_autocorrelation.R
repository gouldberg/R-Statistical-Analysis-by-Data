setwd("//media//kswada//MyFiles//R//cpg")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpg
# ------------------------------------------------------------------------------

data(cpg, package = "astsa")

str(cpg)

cpg



# ------------------------------------------------------------------------------
# Fit linear regression
# ------------------------------------------------------------------------------

fit <- lm(log(cpg) ~ time(cpg))


summary(fit)


par(mfrow = c(2,2))
plot(fit)



# ----------
acf2(resid(fit), max.lag = 20)


# -->
# residuals may be modeled by AR(1) or ARMA(1,2)



