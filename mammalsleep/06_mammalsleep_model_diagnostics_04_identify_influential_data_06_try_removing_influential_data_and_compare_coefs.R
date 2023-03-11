setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)


mod_obj <- qlmod



# ------------------------------------------------------------------------------
# Try removing influential data and compare coefficients
# ------------------------------------------------------------------------------


# removing Asian.elephant and European.hedgehog
qlmod_update <- update(qlmod, subset = -whichNames(c("Asian.elephant", "European.hedgehog"), mammalsleep))



# ----------
compareCoefs(qlmod, qlmod_update)



# -->
# As expected from Added-Variable Plots,
# coefficient for log(body) is decreased,
# coefficient for log(lifespan) and danger are not changed much.
