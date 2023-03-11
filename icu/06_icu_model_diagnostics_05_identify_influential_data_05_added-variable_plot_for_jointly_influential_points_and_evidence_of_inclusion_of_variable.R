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
# marginal plots (spine plots) of the response died against each of the predictors in the model
# ------------------------------------------------------------------------------

op <- par(mfrow = c(2,2), mar = c(4, 4, 1, 2.5) + .1, cex.lab = 1.4)

plot(died ~ cutfac(age), data = ICU2, col = c("lightblue", "pink"))

plot(died ~ cancer, data = ICU2, col = c("lightblue", "pink"))

plot(died ~ admit, data = ICU2, col = c("lightblue", "pink"))

plot(died ~ uncons, data = ICU2, col = c("lightblue", "pink"))


# -->
# Such plots are useful for some purposes, but not for assessing the adequacy of the fitted model



# ------------------------------------------------------------------------------
# Diagnostic plot:  Added-variable plots (partial-regression plots)
#  - Marginal plots of response variable against the predictor variables can conceal or misrepresent the relationships in a model
#    including several predictors together due to correlations or associations among the predictor.
#    Added-variable plots solve this problem by plotting the residuals which are less discrete than the marginal responses in Y
#  - Sets of two (or more) observations can have joint influence, which greatly exceeds their individual influential.
#    Similarly, the influence of one discrepant point can be offset by another influential point in an opposite direction, a phenomenon called masking.
#  - Added-variable plots, showing the partial regression for one predictor controlling or adjusting for all others, can make such cases visually apparent.
# ------------------------------------------------------------------------------

pch <- ifelse(ICU2$died == "Yes", 1, 2)


graphics.off()

car::avPlots(mod_obj, id = TRUE, pch = pch, cex.lab = 1.3)



# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# Close investigation of the evidence for inclusing of "predation" variable
# ------------------------------------------------------------------------------

icu.step2_update <- update(icu.step2, ~ . + systolic)


anova(icu.step2, icu.step2_update, test = "Chisq")



# -->
# THe addition of systolic blood pressure is nearly significant at the conventional alpha = 0.05 level.



# -----------
# Added-variable plot for the effect of adding systolic blood pressure to the main effects model for the ICU data
pch <- ifelse(ICU2$died == "Yes", 1, 20)

par(mfrow=c(1,1))

car::avPlot(icu.step2_update, "systolic", id = TRUE, pch = pch)



# -->
# Cases 331 and 921 have high partial leverage, but they are not influential.
# Case 84, however, has high leverage and a large residual, so it is possibly influential on the evidence for inclusion systolic in the model.
# Note also that the partial regression line in this plot nicely separates nearly all the patients who died from those who survived.
