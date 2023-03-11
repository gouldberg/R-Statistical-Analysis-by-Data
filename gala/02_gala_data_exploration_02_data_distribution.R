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




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(gala)


# -->
# No missing values



# ----------
psych::describe(gala)



# dependent variable

lattice::histogram(gala$Species, breaks=seq(0,500,20))




