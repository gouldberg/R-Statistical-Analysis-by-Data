setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# Poisson regression
# ------------------------------------------------------------------------------

# We just focus on length, area and year
# Including area * year interactions
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))


summary(modp)



# -->
# Significancet tests for the individual coefficients show that all are significant


# ----------
# shortened summary
faraway::sumary(modp)



# ------------------------------------------------------------------------------
# Estimated Coefficient
# ------------------------------------------------------------------------------

# It is somewhat easier to interpres the exponentiated coefficients, exp(beta), as multiplicative effects
# on the expected number and convert these to percentage change, holding other predictors constant.

round(cbind(beta = coef(modp), expbeta = exp(coef(modp)), pct = 100 * (exp(coef(modp)) - 1)), 3)



# -->
# Here, expected intensity by area varangefjord are 2.9 times that of area soroya, a 190.3% increase,
# while year 2000 are 1.23 times that of year 1999, a 23.3% increase.

