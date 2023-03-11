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
# mod_obj <- modp.step2
mod_obj <- modp2
# mod_obj <- mod.nbin


# ------------------------------------------------------------------------------
# removing influential data and compare coefficients
# ------------------------------------------------------------------------------

# removing Asian.elephant and European.hedgehog
mod2_update <- update(mod2, subset = -whichNames(c("Genovesa", "Gardner1", "Rabida"), gala))



# ----------
compareCoefs(mod2, mod2_update)



# -->
# As expected from Added-Variables Plots,
# the coefficients of "Elevation" is changed to larger value