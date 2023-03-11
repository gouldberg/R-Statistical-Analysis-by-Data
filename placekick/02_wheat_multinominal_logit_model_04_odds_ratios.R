setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

levels(wheat$type)


library(nnet)

methods(class = multinom)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)


summary(mod.fit)



# ------------------------------------------------------------------------------
# Odds Ratios for each explanatory variable with 1 unit (standard deviation) change
# ------------------------------------------------------------------------------

# Calculate standard deviation for each variables
summary(wheat)

sd.wheat <- apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)

c.value <- c(1, sd.wheat)  # class = 1 is first value
round(c.value, 2)



# ----------
# Estimated coefficients
# beta.hat_jr for r = 1, ..., 6  and j = 2, 3
beta.hat2 <- coefficients(mod.fit)[1,2:7]
beta.hat3 <- coefficients(mod.fit)[2,2:7]



# ----------
# The estimated odds ratios for each explanatory variable with 1 unit (standard deviation) change
# Odds ratios for j = 2 vs. j = 1 (scab vs. healthy)
round(exp(c.value * beta.hat2),2)
round(1 / exp(c.value * beta.hat2),2)


# -->
# The estimated odds of a scab vs. a healthy kernel change by 0.06 times for a 0.13 increase in the density holding the other variables constant.
# The estimated odds of a scab vs. a healthy kernel change by 17.04 times for a 0.13 decrease in the density holding the other variables constant.



# Odds ratios for j = 3 vs. j = 2 (sprout vs. healthy)
round(exp(c.value * beta.hat3),2)
round(1 / exp(c.value * beta.hat3),2)


# -->
# The estimated odds of a sprout vs. a healthy kernel change by 7.28 times for a 0.13 decrease in the density holding the other variables constant.

