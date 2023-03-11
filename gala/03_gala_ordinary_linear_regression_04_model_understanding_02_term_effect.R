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




# ------------------------------------------------------------------------------
# term plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,3))

termplot(lmod, partial.resid =TRUE, se = TRUE, smooth = panel.smooth, shade = TRUE)




# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)

mod_obj <- lmod


# Main effect (default is response, not linked)
eff <- effects::allEffects(mod_obj)


eff[["Area"]]

eff[["Area"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse)
plot(predictorEffects(mod_obj))

plot(eff, type = "response")




# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))


plot(Effect("Area", mod_obj))




# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

mod_obj <- lmod


# effect plots for several predictors jointly or full-model plots

plot(Effect(c("Area", "Adjacent"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))


plot(Effect(c("Area", "Nearest"), mod_obj), 
     confint = list(style = "bands"), 
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))




# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

mod_obj <- lmod

gala.fitp <- cbind(gala, Species_pred = predict(mod_obj, type = "response"))


head(gala.fitp)



# ----------
library(ggplot2)


graphics.off()

gg <- ggplot(gala.fitp, aes(x = Area, y = Species_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg

gg + facet_grid(~ vcdExtra::cutfac(Nearest, q = 5))

