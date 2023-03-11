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
# Assess proportional odds assumption
#   - by computing the observed odds proportions with respect to, in this case, income levels
# ------------------------------------------------------------------------------

library(faraway)


# compute log-odds difference
( pim <- with(rnes96, prop.table(table(income, party), 1)) )


( tmp <- logit(pim[,1]) - logit(pim[,1] + pim[,2]) )




# ----------
hist(tmp, include.lowest = TRUE)

plot(density(tmp))



# -->
# It is questionable whether these can be considered sufficiently constant, but at least there is no trend.


