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
mod_obj <- modp.step2
# mod_obj <- modp2



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("Area", "Adjacent"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))


plot(Effect(c("Area", "Nearest"), mod_obj), 
     confint = list(style = "bands"), 
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))



