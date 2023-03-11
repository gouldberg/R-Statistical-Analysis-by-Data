setwd("//media//kswada//MyFiles//R//respInf")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  respInf
# ------------------------------------------------------------------------------
data("respInf", package = "gamlss.data")


str(respInf)

car::some(respInf)



# ------------------------------------------------------------------------------
# Fit normal random intercept model to binary response by random()
# ------------------------------------------------------------------------------

m2 <- gamlss(time ~ age + female + cosine + height + random(id), data = respInf, family = BI)


summary(m2)



# ----------
getSmo(m2)



# ------------------------------------------------------------------------------
# Compare m1 (gamlssNP) and m2 (gamlss() + random())
# ------------------------------------------------------------------------------

# comparing the fitted coefficients

coef(m1)

coef(m2)

getSmo(m2)



# -->
# The fitted parameters of models m1 and m2, which are similar except for the fitted random effects parameter which is 0.836 from m1,
# but 1.353 from m2



# ----------
GAIC(m1, m2)


# -->
# Note also that the output from fitting m1 gives the marginal AIC,
# while m2 output gives the conditional AIC (given the fitted random effects parameter gamma).
# The marginal and conditional AICs are not comparable.



# ---------
# term.plot is not applied to m1
# term.plot(m1, pages = 1, ask = FALSE)

term.plot(m2, pages = 1, ask = FALSE)



# -->
# random effects for id appear to be highly positively skewed.
# Perhaps one should try a nonparametric random intercept model


