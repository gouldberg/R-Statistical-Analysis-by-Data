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
# Ordered Probit Model
#   - Assume that the latent variable has a standard normal distribution.
# ------------------------------------------------------------------------------

library(MASS)

opmodi <- polr(party ~ income, method = "probit", rnes96)

opmodi



# ----------
c(deviance(mmod), mmod$edf)
c(deviance(pomodi), pomodi$edf)
c(deviance(opmodi), opmodi$edf)


# -->
# The deviance of orderd probit model is a little smaller than that of proportional odds model



# ----------
summary(pomodi)

summary(opmodi)


# -->
# But the coefficients appear to be different.  (but if rescaled, very similar)



# ------------------------------------------------------------------------------
# Compare predictions
# ------------------------------------------------------------------------------

inclevels <- seq(0, 100, by = 20)

pred_pomodi <- predict(pomodi, data.frame(income = inclevels, row.names = inclevels), type = "probs")
pred_opmodi <- predict(opmodi, data.frame(income = inclevels, row.names = inclevels), type = "probs")

head(pred_pomodi)
head(pred_opmodi)


# -->
# We see that the predicted values are very similar.


( dems <- pnorm(0.128 - inclevels * 0.008182) )
( demind <- pnorm(0.798 - inclevels * 0.008182) )




data.frame(Democrat = dems, Independent)
