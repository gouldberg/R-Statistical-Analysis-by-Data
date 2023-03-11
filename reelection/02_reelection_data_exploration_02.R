setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


summary(Reelection)


psych::describe(Reelection)



# ----------
table(Reelection$reelect, useNA = "always")



# ----------
Hmisc::describe(Reelection)
