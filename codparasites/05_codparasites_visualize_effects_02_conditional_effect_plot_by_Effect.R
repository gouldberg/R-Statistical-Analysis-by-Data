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
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots
eff2 <- Effect(c("area", "length"), modp)


plot(eff2, 
     mutiline = TRUE, ci.stype = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)


plot(eff2, 
     mutiline = TRUE, ci.stype = "bands", type = "response",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)

