setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response, not linked)
eff <- effects::allEffects(modp)


eff[["area"]]

eff[["area"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse)
# For a Poisson GLM, an important feature is that the response is plotted on the log scale, so that effects in the model appear as linear functions.
# while the values of the response (number of articles) are labeled on their original scale, facilitating interpretation.
plot(eff)



# ----------
# this is more comparable among predictors, giving the range of the response on the log scale.
plot(eff, band.colors = "blue", lwd = 3, ylim = c(-2, log(30)))



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
eff2 <- effects::allEffects(modp, partial.residuals = TRUE)
plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



# ----------
# separate representation and not including interactions
plot(Effect("year", modp))

plot(Effect("area", modp))



# ----------
# Multiline type is better !!!
plot(eff[2], type = "response", ylim = c(0, 30),
     multiline = TRUE, ci.style = "bars",
     key.args = list(x = .05, y = .95, columns = 1),
     colors = c("black", "red", "blue"), lty = 1:3,
     symbols = 15:17, cex = 2,
     main = "Poisson model: area * year effect")


# -->
# This helps to interpret the nature of the area by year effet.
# The pattern of mean expected of cod parasites is similar in 1999 and 2001, except for the soroya area.
# The results in year 2000 differ mainly in greater intensity in Tanafjord and Varangerfjord.
# Varangerfjord shows larger infection counts overall, but particularly in year 2000.


