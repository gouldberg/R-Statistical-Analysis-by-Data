setwd("//media//kswada//MyFiles//R//aep")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aep
#  - The data, 1,383 observations, are from a study at the Hospital del Mar, Barcelona, during the years 1988 and 1990 (see Gange et.al)
#    The response variable is the number of inappropariate days (noinap) out of the total number of days (los) patients spent in hospital.
#    Each patient was assessed for inappropriate stay on each day by two @hysicians who used the appropriateness evaluation protocal (AEP).
#
#  - The data were analyzed by Gange et al. and later by Rigby and Stasinopoulos.
#    Gange et al. used a logistic regression model for the number of inappropriate days, with binomial and beta binomial response distributions,
#    and found that the latter provided a better fit to the data.
#    They modelled both the mean and the dispersion of the beta binomial distibution (BB) as functions of explanatory variables using a logit link for mu
#    and an identity link for sigma.
#    Their final model was beta binomial BB(mu, sigma), with terms ward, year and loglos in the model for logit(mu) and year for the sigma model.
#  - variables:
#       - los:  total number of days
#       - noinap:  number of inappropriate days patient stay in hospital
#       - loglos:  the log of los / 10
#       - sex:  the gender of patient
#       - ward:  type of ward in the hospital (medical, surgical or other)
#       - year:  1988 or 1990
#       - age:  age of the patient subtracted from 55
#       - y:  the response variable, a matrix with columns noinap, los-noinap
# ------------------------------------------------------------------------------

data("aep", package = "gamlss.data")


str(aep)

car::some(aep)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

prop <- with(aep, noinap / los)

par(mfrow = c(2,2))

plot(prop ~ age, data = aep, cex = los / 30)
plot(prop ~ sex, data = aep)
plot(prop ~ ward, data = aep)
plot(prop ~ year, data = aep)

