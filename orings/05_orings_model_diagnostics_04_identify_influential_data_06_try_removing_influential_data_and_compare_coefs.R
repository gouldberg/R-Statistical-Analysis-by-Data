setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ----------
mod_obj <- lmod



# ------------------------------------------------------------------------------
# Try removing influential data and compare coefficients
# ------------------------------------------------------------------------------


# removing Asian.elephant and European.hedgehog
lmod_update <- update(lmod, subset = -whichNames(c("1", "18"), orings))



# ----------
compareCoefs(lmod, lmod_update)



# -->
# As expected from Added-Variable Plots,
# coefficient for log(body) is decreased,
# coefficient for log(lifespan) and danger are not changed much.
