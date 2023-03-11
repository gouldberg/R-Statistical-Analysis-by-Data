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
# main effect plot
# ------------------------------------------------------------------------------

library(effects)


plot(allEffects(viet.polr))

plot(allEffects(viet.polr), style = "stacked", key.args = list(x = .55, y = .9))



# ----------
# this is better:  sex and year in different representation
plot(Effect("sex", viet.polr, latent = TRUE))

plot(Effect("year", viet.polr), style = "stacked", key.args = lst(x = .55, y = .9))


# dashed line: thresholds between the adjacent response categories corresponding to the intercepts

