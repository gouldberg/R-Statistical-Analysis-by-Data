setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
Vietnam$response <- ordered(Vietnam$response)



# ------------------------------------------------------------------------------
# Visualize results for proportional odds model
# higher-order effects plot for more focal predictors
# ------------------------------------------------------------------------------

library(effects)


# plot effects of sex and year produced with the argument laten = TRUE
plot(Effect(c("sex", "year"), viet.polr, latent = TRUE), lwd = 3)



# -->
# single line in each panel for the effect (slope) of sex on the log odds
# dashed horizontal lines give the thresholds between the adjacent response categories corresponding to the intercepts
