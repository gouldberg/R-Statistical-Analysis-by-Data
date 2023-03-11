setwd("//media//kswada//MyFiles//R//space_shuttle")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Space Shuttle
# ------------------------------------------------------------------------------
data("SpaceShuttle", package = "vcd")

data <- SpaceShuttle

data



# ------------------------------------------------------------------------------
# Fitting a logistic regression model
# ------------------------------------------------------------------------------
# We focus on the variable nFailures as a binomial with n = 6 trials.
# missing data for flight 4 is excluded by na.action = na.exclude
shuttle.mod <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature, data = data, na.action = na.exclude, family = binomial)



# ----------
# alternative: we can add an explicit trials variable, represent the response as the proportion nFailures / trials, and use weight = trials
# to indicate the total number of observations.
data$trials <- 6

shuttle.modw <- glm(nFailures / trials ~ Temperature, weight = trials, data = data, na.action = na.exclude, family = binomial)


# These two approaches give identical results for all practical purposes
all.equal(coef(shuttle.mod), coef(shuttle.modw))



# ------------------------------------------------------------------------------
# Model tests for simple logistic regression
# ------------------------------------------------------------------------------
# Wald test of the coefficient for age, testing hypothesis H0: beta = 0 
summary(shuttle.mod)



# ----------
# direct test compares the deviance of the fitted model to the deviance of the null model (Chisq test)
# the deviance:  -2 times the log-likelihood ratio of some reduced model to the full model
anova(shuttle.mod, test = "Chisq")



# ------------------------------------------------------------------------------
# plot fitted logistic regression model
# ------------------------------------------------------------------------------
ggplot(data, aes(x = Temperature, y = nFailures / trials)) + xlim(30, 81) + xlab("Temperature (F)") + ylab("O-Ring Failure Probability") + 
  geom_point(position = position_jitter(width = 0, height = 0.01), aes(size = 2)) +
  theme(legend.position = "none") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), fill = "blue", aes(weight = trials), fullrange = TRUE, alpha = 0.2, size = 2)



