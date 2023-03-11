setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Poisson Regression
# ------------------------------------------------------------------------------

# we do not use Endemics here
# If we apply offset to Area, goodness of fit is very low ... so we include Area
# And we could check based on component-plus-residual plot, log transformation for each variable is good

modp <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Adjacent) + log(Scruz + 0.1), data = gala, family=poisson(link="log"))

# modp <- glm(Species ~ log(Area) + log(Adjacent), data = gala, family=poisson(link="log"))



# ----------
summary(modp)



# -->
# It's amazing that all variables have p-value < 0.05, except for log(Elevation)



# ----------
# shortned summary
faraway::sumary(modp)



# ------------------------------------------------------------------------------
# Estimated Coefficient
# ------------------------------------------------------------------------------

# It is somewhat easier to interpres the exponentiated coefficients, exp(beta), as multiplicative effects
# on the expected number and convert these to percentage change, holding other predictors constant.

round(cbind(beta = coef(modp), expbeta = exp(coef(modp)), pct = 100 * (exp(coef(modp)) - 1)), 3)



# -->
# Here, if log(Area) is increased in 1 unit, expected Species is 1.42 times, 41.7% increase
# Here, if log(Adjacent) is increased in 1 unit, expected Species is 0.915 times, -8.5% decrease

# Note that the sing of terms are NOT nonsense.
