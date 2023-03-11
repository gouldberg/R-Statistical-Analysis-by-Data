setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
#  - Data on output and labor and capital inputs for 25 tileries in two regions of Egypt, observed over 12 to 22 years/
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

Tl.p <- pdata.frame(Tileries)



# ----------
# This is unbalanced panel
pdim(Tl.p)



# ----------
head(index(Tl.p))



# ----------
table(index(Tl.p)$id, useNA = "always")

table(index(Tl.p)$week, useNA = "always")

table(index(Tl.p))


