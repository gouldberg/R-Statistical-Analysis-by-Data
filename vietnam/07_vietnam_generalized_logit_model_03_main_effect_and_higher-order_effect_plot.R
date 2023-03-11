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
levels(Vietnam$response)


# choose "A" clearly as baseline category
Vietnam$response <- relevel(Vietnam$response, ref = "A")

levels(Vietnam$response)



# ------------------------------------------------------------------------------
# Main effect plot (higher-order effect plot)
# ------------------------------------------------------------------------------

library(effects)


plot(allEffects(viet.multinom2))

plot(allEffects(viet.multinom2), style = "stacked", key.args = list(x = .55, y = .9))

