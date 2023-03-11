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
# Proportional odds model
# ------------------------------------------------------------------------------

# Order properly the type factor
levels(wheat$type)

wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))

levels(wheat$type.order)



# ----------
library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
# mod.fit.ord2 <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ------------------------------------------------------------------------------
# Odds ratios
# ------------------------------------------------------------------------------

# Information about each variable to help with choosing c
summary(wheat)

sd.wheat <- apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
c.value <- c(1, sd.wheat)
round(c.value, 2)  # class = 1 is first value



# ----------
# Odds ratios
round(exp(c.value * (-mod.fit.ord$coefficients)),2)
round(1 / exp(c.value * (-mod.fit.ord$coefficients)),2)


# -->
# The estimated odds of a scab (Y <= 1) vs. sprout or healthy (Y > 1) response are 0.84 times as large for soft rather than hard red winter wheat,
# holding the other variables constant.

# The estimated odds of a scab (Y <= 1) vs. sprout or healthy (Y > 1) response change by 5.89 times for a 0.13 decrease in the density, holding the other variables constant.

# The estimated odds of a scab (Y <= 1) vs. sprout or healthy (Y > 1) response change by 2.74 times for a 7.92 decrease in the weight, holding the other variables constant.


# -->
# Because of the proportional odds assumtion, each of these statements ARE APPLIES to the odds of a scab or sprout vs. healthy response.
# For this reason, it is common to interpret odds ratios, such as for density, by saying:
#   - The estimated odds of kernel quality being below a particular level change by 5.89 times for a 0.13 decrease in the density, holding the other variables constant.


