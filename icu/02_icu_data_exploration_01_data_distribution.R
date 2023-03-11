setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# Data Exploration:  data distribution
# ------------------------------------------------------------------------------

Hmisc::describe(ICU)


psych::describe(ICU)


# Try but Not recommended ...
# GGally::ggpairs(ICU)