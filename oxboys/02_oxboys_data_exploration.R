setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

check_index(d$Subject)



# ----------
summary(d)


xtabs(~ Occasion, data = d)



# ---------
psych::describe(d)


Hmisc::describe(d)

