]setwd("//media//kswada//MyFiles//R//gala")

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
# models
# ------------------------------------------------------------------------------

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)

lmod_final <- lm(Species ~ Elevation + Adjacent, data = gala)

lmod_r_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala[-16,])




# ------------------------------------------------------------------------------
# Stepwise selection
# ------------------------------------------------------------------------------

var_obj <- c("Area", "Elevation", "Nearest", "Scruz", "Adjacent")



lmod_step <- step(lmod, direction = "both")


summary(lmod_step)



# -->
# Same terms "Elevation" and "Adjacent" are selected.

