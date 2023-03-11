setwd("//media//kswada//MyFiles//R//arbuthnot")

packages <- c("dplyr", "vcd", "MASS", "HistData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arbuthnot data
# ------------------------------------------------------------------------------

data("Arbuthnot", package = "HistData")


str(Arbuthnot)


dim(Arbuthnot)


car::some(Arbuthnot)



# ------------------------------------------------------------------------------
# Fit logistic regression for the proportion of male births with other variables
#   - Asess whether the proportion of male births varied with the variables Year, Plague, and Mortality
# ------------------------------------------------------------------------------

arb.mod <- glm(Males/(Males+Females) ~ Year + Plague + Mortality, data = Arbuthnot, weights = Males+Females, family = binomial)


summary(arb.mod)



# ----------
lmtest::coeftest(arb.mod)



# ----------
# testing vs. null model
anova(arb.mod, test = "Chisq")


