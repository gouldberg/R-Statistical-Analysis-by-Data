setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)

jsp.p <- pdata.frame(jsp, index = c("id", "year"))



# ----------
# This is unbalanced panel
pdim(jsp.p)



# ----------
head(index(jsp.p))



# ----------
table(index(jsp.p)$id, useNA = "always")

table(index(jsp.p)$year, useNA = "always")

table(index(jsp.p))


