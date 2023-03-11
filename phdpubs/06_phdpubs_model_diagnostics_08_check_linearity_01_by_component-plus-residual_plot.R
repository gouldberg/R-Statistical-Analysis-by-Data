setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Diagnostic plot:  Component-plus-residual plots (partial residual plot)
#  - Designed to show whether a given quantiative predictor x included linearly in the model, actually shows a nonlinear relation, requiring transformation.
#  - Most useful when there are several quantitative predictors, so that it is convenient and sensible to examine their relationships individually.
#  - The essential idea is to move the linear term for x back into the residual, by calculating the partial residuals.
# ------------------------------------------------------------------------------

summary(modp)

car::crPlots(modp, ~ mentor, smooth = list(span = 0.5),
             id = TRUE, showLabels = list(method=list(abs(residuals(modp, type="pearson")), "x"), n=2, cex=1, col=car::carPalette()[1], location="lr"))



# -->
# The dashed red line:  the slope of age in the full model, the smoothed green curve shows a loess fit with span = 0.5.
# The smoothed loess curve:  closely resembles the trend we saw in the conditional plot for age by sex,
# suggesting the need to include a nonlinear term for age.

# The points identified in this plot, by default:  either the most extreme x values (giving them high leverage)
# or the largest absolute Pearson residuals in the full model.

# BUT please note that crPlots() and crPlot() does not work for those with interaction terms


# -->
# component added-plot shows no particular non-linear relationship between mentor and dependend variables

