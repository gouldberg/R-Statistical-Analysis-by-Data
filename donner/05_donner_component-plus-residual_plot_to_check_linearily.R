setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))

fam <- data$family
levels(fam)[c(3,4,6,7,9)] <- "Others"

fam <- factor(fam, levels(fam)[c(1, 2, 4:6, 3)])
data$family <- fam



# ----------
donner.mod1 <- glm(survived ~ age + sex, data = data, family = binomial)
donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Diagnostic plot:  Component-plus-residual plots (partial residual plot)
#  - Designed to show whether a given quantiative predictor x included linearly in the model, actually shows a nonlinear relation, requiring transformation.
#  - Most useful when there are several quantitative predictors, so that it is convenient and sensible to examine their relationships individually.
#  - The essential idea is to move the linear term for x back into the residual, by calculating the partial residuals.
# ------------------------------------------------------------------------------
# The dashed red line shows the slope of age in the full model, the smoothed green curve shows a loess fit with span = 0.5.
# The smoothed loess curve in this plot closely resembles the trend we saw in the conditional plot for age by sex,
# suggesting the need to include a nonlinear term for age.
# The points identified in this plot, by default, are those with either the most extreme x values (giving them high leverage)
# or the largest absolute Pearson residuals in the full model.

# BUT please note that crPlots() and crPlot() does not work for those with interaction terms
car::crPlots(donner.mod1, ~ age, smooth = list(span = 0.5),
             id = TRUE, showLabels = list(method=list(abs(residuals(donner.mod1, type="pearson")), "x"), n=2, cex=1, col=car::carPalette()[1], location="lr"))

# -->
# The four structured bands of points in the plot correspond to the combinations of sex and survival.



# Syntax requires that you specify a term in the model, rather than just a predictor variable
car::crPlots(donner.mod3, ~ poly(age,2), smooth = list(span = 0.5),
             id = TRUE, showLabels = list(method=list(abs(residuals(donner.mod1, type="pearson")), "x"), n=2, cex=1, col=car::carPalette()[1], location="lr"))



# --> Except possibly at the extreme right, this plot shows no indication of a (further) nonlinear relationship



