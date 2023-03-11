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
# Hypothesis tests by Wald tests
#   - H0:  beta(21) = 0  vs. beta(21) != 0    (21: class variable in the log-odds for scab vs. healthy)
# ------------------------------------------------------------------------------

# standard errors for the coefficient of "classrw" for Scab
sqrt(vcov(mod.fit)[2,2])



# Wald tests for one beta - usually not done since it tests only one parameter
sum.fit <- summary(mod.fit)
test.stat <- sum.fit$coefficients/sum.fit$standard.errors

p.value <- 2 * (1 - pnorm(q = abs(test.stat), mean = 0, sd = 1))
round(test.stat,2)
round(p.value,2)



# ------------------------------------------------------------------------------
# Hypothesis tests by Likelihood Ratio Test
#   - H0:  beta(21) = 0  vs. beta(21) != 0    (21: class variable in the log-odds for scab vs. healthy)
# ------------------------------------------------------------------------------

# LRT for class:
mod.fit.Ho <- multinom(formula = type ~ density + hardness + size + weight + moisture, data=wheat)

anova(mod.fit.Ho, mod.fit)   


# ----------
# Additional code showing the LRT
G.sq.Ho <- mod.fit.Ho$deviance 
G.sq.Ha <- mod.fit$deviance 
G.sq <- G.sq.Ho-G.sq.Ha
p.value <- 1 - pchisq(q = G.sq, df = 2)

data.frame(G.sq.Ho, G.sq.Ha, G.sq, p.value, df = 2)



# ------------------------------------------------------------------------------
# Hypothesis tests by Likelihood Ratio Test
#   - Does class of wheat have "any" effect on the probabilities of healthy, scab, or sprout kernels ?
#   - H0:  beta(21) = beta(31) = 0  vs. beta(21) != 0 and/or beta(31) != 0
# ------------------------------------------------------------------------------

library(package = car)

Anova(mod.fit)


# -->
# The transformed test statistic is -2 * log() = 0.964, and the corresponding p-value is 0.6175 using a X^2 distributional approximation.
# There is not sufficient evidence to indicate that the class of wheat is important given that the other variables are in the model.

# Separate tests for density, hardness, and weight in the output all indicate at least marginal evidence of importance
# for these explanatory variables.

qchisq(p = 0.95, df = 2)

