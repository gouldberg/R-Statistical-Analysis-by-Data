setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
icu.step2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)

mod_obj <- icu.step2



# ------------------------------------------------------------------------------
# Diagnostic plot:  Component-plus-residual plots (partial residual plot)
#  - Designed to show whether a given quantiative predictor x included linearly in the model, actually shows a nonlinear relation, requiring transformation.
#  - Most useful when there are several quantitative predictors, so that it is convenient and sensible to examine their relationships individually.
#  - The essential idea is to move the linear term for x back into the residual, by calculating the partial residuals.
# ------------------------------------------------------------------------------
# Diagnostic plot:  CERES plots (Combining conditional Expectation and RESiduals)
#  - If the regressions among the predictors are strongly nonlinear and not well described by polynomials,
#    then component-plus-residuals plots may not be effective, in recovering nonlinear partial relationships between
#    the response nad the predictors.
#    CERES plots use nonparametric-regression smoothers rather than polynomial regressions
#    to adjust for nonlinear relationships among the predictors
#  - Experience suggests that nonlinear relationships among the predictors induce problemns for component-plus-residual plots
#    only when these relationships are strong. In such cases, a component-plus-residual plot can appeear nonlinear even when the true
#    partial regression is linear  -- a phenomenon termed leakage.
# ------------------------------------------------------------------------------

summary(mod_obj)


par(mfrow = c(1,2))

car::crPlots(mod_obj, ~ age, smooth = list(span = 0.5), id = TRUE)

# here no numeric covariates other than age
# car::ceresPlots(mod_obj, ~ age, smooth = list(span = 0.5), id = TRUE)


car::crPlots(mod_obj, ~ cancer, smooth = list(span = 0.5), id = TRUE)

car::crPlots(mod_obj, ~ admit, smooth = list(span = 0.5), id = TRUE)

car::crPlots(qlmod, ~ uncons, smooth = list(span = 0.5), id = TRUE)



# -->
# The dashed red line:  the slope of age in the full model, the smoothed green curve shows a loess fit with span = 0.5.
# The smoothed loess curve

# The points identified in this plot, by default:  either the most extreme x values (giving them high leverage)
# or the largest absolute Pearson residuals in the full model.

# BUT please note that crPlots() and crPlot() does not work for those with interaction terms


# -->
# component added-plot suggest somewhat non-linearity for log(body) relationship with dependent variables



# ----------
# after adding cubic poly terms 
car::crPlots(mod_obj, ~ poly(age,3), smooth = list(span = 0.5), id = TRUE)


