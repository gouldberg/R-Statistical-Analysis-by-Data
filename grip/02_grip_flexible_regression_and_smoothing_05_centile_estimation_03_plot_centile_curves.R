setwd("//media//kswada//MyFiles//R//grip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  grip
# ------------------------------------------------------------------------------

data("grip", package = "gamlss.data")


str(grip)

car::some(grip)



# ------------------------------------------------------------------------------
# plot fitted centile curves for y against x for different centile percentages
# ------------------------------------------------------------------------------

centiles(m0, grip$age, legend = FALSE)


centiles(m1_s2, grip$age, legend = FALSE)


centiles(m2_s2, grip$age, legend = FALSE)


# -->
# Note that the sample percentage of observations below each of the fited centile curves from the fitted model
# are printed (at the end of each line,
# so comparisons with nominal model centiles (printed after below on each line)



# ------------------------------------------------------------------------------
# Split fitted centile curves according to different cut points in the x-variable
# ------------------------------------------------------------------------------

# cut at age 12,14
centiles.split(m0, xvar = grip$age, xcut.points = c(12,14))


centiles.split(m1_s2, xvar = grip$age, xcut.points = c(12,14))


centiles.split(m2_s2, xvar = grip$age, xcut.points = c(12,14))




