setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ----------
elect.l <- glm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
               data = Reelection,
               family = "binomial", subset = narrow)


elect.p <- update(elect.l, family = binomial(link = "probit"))



mod_obj <- elect.l

# mod_obj <- elect.p



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("gdppc", "dev"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))



plot(Effect(c("gdppc", "maj"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))



plot(Effect(c("dev", "maj"), mod_obj), 
     confint = list(style = "bars"),
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))
