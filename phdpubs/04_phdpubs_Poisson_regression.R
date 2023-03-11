setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Poisson regression
# ------------------------------------------------------------------------------

modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))


summary(modp)



# -->
# Significancet tests for the individual coefficients show that all are significant, except for "phdprestige".



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
# Here, expected publications by married candidates are 1.17 times that of non-married, a 17% increase,
# while each additional child multiplies articles by 0.831, a 16.88% decrease.

