setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


summary(attenu)


# -->
# there are 16 records with station missing values



# -----------
psych::describe(attenu)




# ----------
xtabs(~ station + event, data = attenu)



# -->
# events are recorded at only some stations