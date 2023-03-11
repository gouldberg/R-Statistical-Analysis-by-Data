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
# Re-analysis using smoothers for one or both of the continuous variables age and height
# ------------------------------------------------------------------------------

m21 <- gamlss(time ~ pb(age) + female + cosine + pb(height) + random(id), data = respInf, family = BI, trace = FALSE)


m22 <- gamlss(time ~ age + female + cosine + pb(height) + random(id), data = respInf, family = BI, trace = FALSE)


m23 <- gamlss(time ~ pb(age) + female + cosine + height + random(id), data = respInf, family = BI, trace = FALSE)



AIC(m2, m21, m22, m23)



# -->
# According to the AIC, model m23, which has a smoother for age only, is preferred.



# ---------
term.plot(m2, pages = 1, ask = FALSE)

term.plot(m23, pages = 1, ask = FALSE)



# -->
# random effects for id appear to be highly positively skewed.
# Perhaps one should try a nonparametric random intercept model

