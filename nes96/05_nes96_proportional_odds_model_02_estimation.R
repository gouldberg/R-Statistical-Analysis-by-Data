setwd("//media//kswada//MyFiles//R//nes96")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nes96
# ------------------------------------------------------------------------------

data("nes96", package = "faraway")

str(nes96)

head(nes96)



# ----------
party <- nes96$PID

levels(party)


# We collapse this to three
levels(party) <- c("Democrat", "Democrat", "Independent", "Independent", "Independent", "Republican", "Republican")


# The income variable in the original data was an ordered factor with income ranges.
# We have converted this to a numeric variable by taking the midpoint of each range.
inca <- c(1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115)

income <- inca[unclass(nes96$income)]



# For simplicity, we consider only the age, education level and income group of the respondents
rnes96 <- data.frame(party, income, education = nes96$educ, age = nes96$age)


car::some(rnes96)



# ------------------------------------------------------------------------------
# Proportional Odds Model
# ------------------------------------------------------------------------------

# We assume that Independents fall somewhere between Democrats and Republicans.

library(MASS)

pomod <- polr(party ~ age + education + income, rnes96)

pomod



# ----------
c(deviance(mmod), mmod$edf)

c(deviance(pomod), pomod$edf)


pchisq(deviance(mmod) - deviance(pomod), mmod$edf - pomod$edf, lower = FALSE)



# -->
# The proportiona odds model uses fewer parameters, but does not fit quite as well.



# ------------------------------------------------------------------------------
# Model selection by AIC-based
# ------------------------------------------------------------------------------

pomodi <- step(pomod, direction = "both")

pomodi



# ----------
# Likelihood ratio test to compare the models
c(deviance(pomod), pomod$edf)

c(deviance(pomodi), pomodi$edf)

pchisq(deviance(pomodi) - deviance(pomod), pomod$edf - pomodi$edf, lower = FALSE)



# -->
# We see that the simplification to just income is justifiable.



# ------------------------------------------------------------------------------
# Compare with other models
# ------------------------------------------------------------------------------

c(deviance(mmodi), mmodi$edf)

c(deviance(pomodi), pomodi$edf)


pchisq(deviance(pomodi) - deviance(mmodi), mmodi$edf - pomodi$edf, lower = FALSE)



# -->
# Proportional odds model increases deviance, but edf is decreased by 1, offsetting sufficiently the deviance increase.


