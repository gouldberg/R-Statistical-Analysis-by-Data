setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(d)


# -->
# There are many variables with many NAs
# For "rgdppc_2000", 64 records are NAs



# ----------
psych::describe(d)


