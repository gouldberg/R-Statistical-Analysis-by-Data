setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# simple linear model for reference purposes
# ------------------------------------------------------------------------------
# olm <- lm(NOx ~ C + E, ethanol)
olm <- lm(NOx ~ C * E, ethanol)

summary(olm)


# -->
# interaction terms are not significant



