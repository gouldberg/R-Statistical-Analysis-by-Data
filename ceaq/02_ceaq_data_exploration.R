setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(CEAQ)


apply(CEAQ, 2, FUN = table)



# -->
# all variables are 3 values interger  (rating scale by 3)


