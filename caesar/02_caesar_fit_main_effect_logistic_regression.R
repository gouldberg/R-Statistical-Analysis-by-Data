setwd("//media//kswada//MyFiles//R//caesar")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Caesar
#   - 3 * 2^3 frequency table classifying 251 women who gave birth by Caesarian section by Infection (three levels: none, Type 1, Type 2)
#     and Risk, whether Antibiotics were used, and whether the Caesarian section was Planned or not.
# ------------------------------------------------------------------------------
data("Caesar", package = "vcdExtra")

Caesar


# Convert to data frame
data <- as.data.frame(Caesar)


# Here consider only the binary outcome of infection vs. no infection
data$Infect <- as.numeric(data$Infection %in% c("Type 1", "Type 2"))


data



# ----------
# recode factor level
data$Risk <- factor(data$Risk, level = c("No", "Yes"))
data$Antibiotics <- factor(data$Antibiotics, level = c("No", "Yes"))
data$Planned <- factor(data$Planned, level = c("No", "Yes"))



# ------------------------------------------------------------------------------
# Multiple logistic regression models
#  - The parameters defined hear are incremental effects.
# ------------------------------------------------------------------------------
# Check the factor level
str(data)


cae.mod <- glm(Infect ~ Risk + Antibiotics + Planned, data = data, weights = Freq, family = binomial)



# ----------
summary(cae.mod)
lmtest::coeftest(cae.mod)



# ------------------------------------------------------------------------------
# Confidential intervals of odds ratio
# ------------------------------------------------------------------------------
round(exp(cbind(OddsRatio = coef(cae.mod), confint(cae.mod))), digits = 4)




# ------------------------------------------------------------------------------
# Model tests
# ------------------------------------------------------------------------------
# Wald test of the coefficient for age, testing hypothesis H0: beta = 0 
summary(cae.mod)



# ----------
# direct test compares the deviance of the fitted model to the deviance of the null model (Chisq test)
# the deviance:  -2 times the log-likelihood ratio of some reduced model to the full model
anova(cae.mod, test = "Chisq")



# ----------
car::Anova(cae.mod)



# ------------------------------------------------------------------------------
# Effect plots
#  - For more than two variables, full-model plots of the fitted response surface can be cumbersome, particularly when the model contains
#    interactions or when the main substantive interest is focused on a given main effect or interaction, controlling for all other explanatory variables.
#  - All other variables are "controlled" by being fixed at typical values.
# ------------------------------------------------------------------------------
# Main effects model's main effect
cae.eff <- effects::allEffects(cae.mod, partial.residuals = TRUE)
names(cae.eff)



# ----------
# Plot of all effects (each high-order term) in the main effects model
graphics.off();
plot(cae.eff, rows = 1, cols = 3, type="link", residuals.pch = 15)



# ----------
# Full-model plot of the effects of all predictors in the main effects model (on logit scale)
( cae.full <- effects::Effect(c("Risk", "Antibiotics", "Planned"), cae.mod) )

plot(cae.full, multiline = TRUE, colors = c("red", "blue"), lwd = 3, grid = TRUE)


# on scale of probabilities
plot(cae.full, multiline = TRUE, type="response", colors = c("red", "blue"), lwd = 3, grid = TRUE)

