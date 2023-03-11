setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# mod_obj <- modp.step2
mod_obj <- modp2
# mod_obj <- mod.nbin


# ------------------------------------------------------------------------------
# marginal plots (spine plots) of the response died against each of the predictors in the model
# ------------------------------------------------------------------------------

op <- par(mfrow = c(2,2), mar = c(4, 4, 1, 2.5) + .1, cex.lab = 1.4)

plot(Species ~ cutfac(log(Area)), data = gala, col = c("lightblue", "pink"))

plot(Species ~ cutfac(log(Adjacent)), data = gala, col = c("lightblue", "pink"))

plot(Species ~ cutfac(log(Nearest)), data = gala, col = c("lightblue", "pink"))

plot(Species ~ cutfac(log(Scruz + 0.1)), data = gala, col = c("lightblue", "pink"))



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

pch <- ifelse(gala$Specis >= 20, 1, 2)


graphics.off()

car::avPlots(mod_obj, id = TRUE, pch = pch, cex.lab = 1.3)



# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# Close investigation of the evidence for inclusing of "Elevation" variable
# ------------------------------------------------------------------------------

mod2 <- glm(Species ~ log(Area) + log(Adjacent + 0.1) + log(Nearest) + log(Scruz + 0.1) + log(Elevation), data = gala, family = poisson(link="log"))


anova(mod_obj, mod2, test = "Chisq")



# -->
# The addition of predation is NOT significant


pch <- ifelse(gala$Species >= 20, 1, 20)

par(mfrow=c(1,1))

car::avPlot(mod2, "log(Elevation)", id = TRUE, pch = pch)


