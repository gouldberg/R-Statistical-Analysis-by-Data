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
# Non-proportional odds model
# ------------------------------------------------------------------------------

library(VGAM)

nmodi <- vglm(party ~ income, family = cumulative(parallel = FALSE), rnes96)

nmodi

summary(nmodi)



# ----------
# for comparison, proportional odds model by vglm
pmodi <- vglm(party ~ income, family = cumulative(parallel = TRUE), rnes96)



# ----------
1 - pchisq(deviance(pmodi) - deviance(nmodi), 1)



# -->
# There is a difference of only one parameter between the two models.
# We see that the simplification to the proportional odds model is not justified here.
# However, we should bear in mind that we have a fairly large sample size of almost 1000 cases so even relatively small differences are prone to be significatn.



# ------------------------------------------------------------------------------
# Model understanding
# ------------------------------------------------------------------------------

# Nonparallel lines will cross somewhere, here
(1.148270 - 0.328868) / (0.010486 - 0.016186)


# So at an income below -143.7547, the predicted probability of being Democrat would exceed the probability of being Democrat or Independent.
# Clearly this is impossible and the model predictions are ridiculous.


