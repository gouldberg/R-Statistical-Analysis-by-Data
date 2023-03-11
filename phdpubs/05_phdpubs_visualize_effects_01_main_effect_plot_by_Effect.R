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
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response, not linked)
eff <- effects::allEffects(modp)


eff[["married"]]

eff[["married"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse)
# For a Poisson GLM, an important feature is that the response is plotted on the log scale, so that effects in the model appear as linear functions.
# while the values of the response (number of articles) are labeled on their original scale, facilitating interpretation.
plot(eff)



# ----------
# this is more comparable among predictors, giving the range of the response on the log scale.
plot(eff, band.colors = "blue", lwd = 3, ylim = c(0, log(10)))



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
eff2 <- effects::allEffects(modp, partial.residuals = TRUE)
plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



# ----------
# separate representation
plot(Effect("kid5", modp))

plot(Effect("kid5", modp))


