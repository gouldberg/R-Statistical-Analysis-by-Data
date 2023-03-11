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
modp2 <- update(modp, . ~ . - phdprestige)



# ------------------------------------------------------------------------------
# Diagnostic plot:  Added-variable plots (partial-regression plots)
#  - Marginal plots of response variable against the predictor variables can conceal or misrepresent the relationships in a model
#    including several predictors together due to correlations or associations among the predictor.
#    Added-variable plots solve this problem by plotting the residuals which are less discrete than the marginal responses in Y
#  - Sets of two (or more) observations can have joint influence, which greatly exceeds their individual influential.
#    Similarly, the influence of one discrepant point can be offset by another influential point in an opposite direction, a phenomenon called masking.
#  - Added-variable plots, showing the partial regression for one predictor controlling or adjusting for all others, can make such cases visually apparent.
# ------------------------------------------------------------------------------

pch <- ifelse(PhdPubs$articles >= 2, 1, 2)


graphics.off()

car::avPlots(modp2, id = TRUE, pch = pch, cex.lab = 1.3)


# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# Close investigation of the evidence for inclusing of systolic variable
# ------------------------------------------------------------------------------

anova(modp, modp2, test = "Chisq")


# -->
# The addition of phdprestige is Not significant at the conventional alpha = 0.05 level.


par(mfrow=c(1,1))

car::avPlot(modp, "phdprestige", id = TRUE, pch = pch)



