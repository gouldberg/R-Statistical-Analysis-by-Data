setwd("//media//kswada//MyFiles//R//arrests")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra", "carData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arrests
# ------------------------------------------------------------------------------
data("Arrests", package = "carData")

dim(Arrests)
str(Arrests)


car::some(Arrests)


data <- Arrests



# To allow for possible nonlinear effects of year, this variable was treated as a factor rather than as a (linear) numeric variable
data$year <- as.factor(data$year)



# ------------------------------------------------------------------------------
# fit logistic regression model including interactions
# ------------------------------------------------------------------------------
arrests.mod <- glm(released ~ employed + citizen + checks + colour*year + colour*age, family = binomial, data = data)



# ----------
# Type II tests of each effect (significance tests for the model terms)
car::Anova(arrests.mod)



# ----------
summary(arrests.mod)

lmtest::coeftest(arrests.mod)

round(exp(coef(arrests.mod)), digits = 3)



# -->
# By direct calculation, you can find that the odds of a quick release was exp(0.735) = 2.08 times greater for someone employed
# exp(0.586) = 1.80 times more likely for a Canadian citizen
# exp(1.21) = 3.36 times more likely for a white than a black person.
# It is much more difficult to interpret the interaction terms.

