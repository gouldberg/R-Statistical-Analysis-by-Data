setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data


# The response variable "Improved" has three categories (none, some, or marked improvement), but for now we consider whether the patient showed
# any improvement at all, defining the event Better to be some or marked improvement.
data$Better <- as.numeric(data$Improved > "None")



# ------------------------------------------------------------------------------
# Multiple logistic regression models
#  - The parameters defined here are incremental effects.
#    The intercept corresponds to a baseline group (50-year-old females given the placebo);  the other parameters are incremental effects for
#    the other groups compared to the baseline group.
# ------------------------------------------------------------------------------
# Check the factor level
str(data)


# To make the intercept interpretable, we will fit the model centering age near the mean by using x - 50 as the first regressor.
arth.logistic2 <- glm(Better ~ I(Age-50) + Sex + Treatment, data = data, family = binomial)



# ----------
summary(arth.logistic2)
lmtest::coeftest(arth.logistic2)



# ------------------------------------------------------------------------------
# Confidential intervals of odds ratio
# ------------------------------------------------------------------------------
# At age 50, females given the placebo have and odds of improvement of 0.56
# Each year of age mutiplies the odds of improvement by 1.05 or a 5% increase.
# Males are only 0.23 times as likely to show improvement relative to female.
# People given the active treatment are 4.8 times more likely to show improvement compared to those given the placebo.
round(exp(cbind(OddsRatio = coef(arth.logistic2), confint(arth.logistic2))), digits = 4)



# ------------------------------------------------------------------------------
# conditional plots:  Placebo vs. Treated
#
#  - The predictors that are not shown in a given plot are effectively ignored (or marginalized)
# ------------------------------------------------------------------------------
gg <- ggplot(data, aes(Age, Better, color = Treatment)) + xlim(5, 95) + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), alpha = 0.2, aes(fill = Treatment), size = 2.5, fullrange = TRUE)
gg




# ----------
# conditional plots:  Placebo vs. Treated faceted by Sex
# The panel for males shows a paradoxical negative relation with age for the treated group and a step function for the placebo group.
gg + facet_wrap(~ Sex)


# Less than 1/3 of the sample were males, and of these only 11 were in the placebo group.
# glm() cannot estimate the fitted relationship against Age here, the slope coefficient is infinite, and the fitted probabilities are all either 0 or 1.
addmargins(xtabs(~Sex + Treatment, data = data), 2)



# ------------------------------------------------------------------------------
# Full-model plots
#  - For a model with two or more explanatory variables, full-model plots display the fitted response surface for all predictors together,
#    rather than stratified by conditioning variables.
#  - Such plots show the predicted values for the response variable on the ordinate against one chosen predictor on the abscissa,
#    and can use multiple curves and multiple panels to represnet other predictors.
#  - This plot method is designed to use a numeric predictor as the horizontal axis,
#    and show separate point symbols and curves for the levels of the combinations of factors (if any)
# ------------------------------------------------------------------------------
# type = "link": logit scale   type = "response": the probability scale (default)
# It provides a visual representation of the information contained in the table of coefficients
# Plotting on logit scales shows the additive linear effects of all predictors
graphics.off()
vcd::binreg_plot(arth.logistic2, type = "link")



# ----------
# by sex
# The choice to display Treatment within each panel makes it easier to judge the size of this effect, compared to the effect of Sex, which must be judged across the panels.
# It shows the data as points, and the fitted lines and confidence bands are restricted to the range of the data in each.
# We can easily see the reason for the unusual pattern in the conditional plot for males.
vcd::binreg_plot(arth.logistic2, type = "link", subset = Sex == "Female", main = "Female", xlim=c(25, 75), ylim = c(-3, 3))
vcd::binreg_plot(arth.logistic2, type = "link", subset = Sex == "Male", main = "Male", xlim=c(25, 75), ylim = c(-3, 3))


# probability scales (default)
vcd::binreg_plot(arth.logistic2, subset = Sex == "Female", main = "Female", xlim = c(25, 75))
vcd::binreg_plot(arth.logistic2, subset = Sex == "Male", main = "Male", xlim = c(25, 75))



# ------------------------------------------------------------------------------
# Effect plots
#  - For more than two variables, full-model plots of the fitted response surface can be cumbersome, particularly when the model contains
#    interactions or when the main substantive interest is focused on a given main effect or interaction, controlling for all other explanatory variables.
#  - All other variables are "controlled" by being fixed at typical values.
# ------------------------------------------------------------------------------
# Main effects model's main effect
arth.eff2 <- effects::allEffects(arth.logistic2, partial.residuals = TRUE)
names(arth.eff2)



# ----------
# Main effect for Sex
arth.eff2[["Sex"]]
arth.eff2[["Sex"]]$model.matrix



# ----------
# Plot of all effects (each high-order term) in the main effects model
# Partial residuals and their loess smooth are also shown for the continuous predictor, Age.
# Shows a hint of non-linearity, but perhaps not enough to worry about.
graphics.off();
plot(arth.eff2, rows = 1, cols = 3, type="response", residuals.pch = 15)



# ----------
# Full-model plot of the effects of all predictors in the main effects model (on logit scale)
( arth.full <- effects::Effect(c("Age", "Treatment", "Sex"), arth.logistic2) )

plot(arth.full, multiline = TRUE, ci.style = "bands", colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)), key.args = list(x = .52, y = .92, columns = 1),  grid = TRUE)


# on scale of probabilities
plot(arth.full, multiline = TRUE, ci.style = "bands", type="response", colors = c("red", "blue"), lwd = 3, key.args = list(x = .52, y = .92, columns = 1),grid = TRUE)

