setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)

mod_obj <- qlmod



# ------------------------------------------------------------------------------
# marginal plots (spine plots) of the response died against each of the predictors in the model
# ------------------------------------------------------------------------------

op <- par(mfrow = c(2,2), mar = c(4, 4, 1, 2.5) + .1, cex.lab = 1.4)

plot(pdr ~ cutfac(log(body)), data = mammalsleep, col = c("lightblue", "pink"))

plot(pdr ~ cutfac(log(lifespan)), data = na.omit(mammalsleep), col = c("lightblue", "pink"))

plot(pdr ~ danger, data = mammalsleep, col = c("lightblue", "pink"))



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

pch <- ifelse(mammalsleep$danger >= 3, 1, 2)


graphics.off()

car::avPlots(qlmod, id = TRUE, pch = pch, cex.lab = 1.3)



# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# Close investigation of the evidence for inclusing of "predation" variable
# ------------------------------------------------------------------------------

qlmod2 <- glm(pdr ~ log(body) + log(lifespan) + danger + predation, data = mammalsleep, family = quasibinomial)


anova(qlmod, qlmod2, test = "Chisq")



# -->
# The addition of predation is NOT significant

pch <- ifelse(mammalsleep$pdr >= 0.2, 1, 20)

par(mfrow=c(1,1))

car::avPlot(qlmod2, "predation", id = TRUE, pch = pch)


