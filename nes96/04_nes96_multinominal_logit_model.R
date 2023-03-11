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
# multinominal logit model
#   - Observed party affiliation proportions are statistially significant ?
# ------------------------------------------------------------------------------

library(nnet)

mmod <- multinom(party ~ age + education + income, rnes96)


summary(mmod)




# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------

mmodi <- step(mmod, direction = "both")

summary(mmodi)



# -->
# At the first stage of the search, we see that omitting education would be the best option to reduce the AIC criterion.
# At the next step, age is removed resulting in a model with only income.



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

mmode <- multinom(party ~ age + income, rnes96)


# Deviance test indicates we should drop 'age' from the model
pchisq(deviance(mmode) - deviance(mmod), mmod$edf - mmode$edf, lower = FALSE)



# -->
# We see that education is not significant relative to the full model.
# This may seem some what surprising, but the large differences between proportions of Democrats and Republicans occur for groups with low education
# which represent only a small number of people.


