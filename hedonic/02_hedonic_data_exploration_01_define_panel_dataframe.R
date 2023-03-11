setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)


car::some(Hedonic)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


Hp <- pdata.frame(Hedonic, index = "townid")



# ----------
# This is unbalanced panel
# This is not really panel, not time index
pdim(Hp)



# ----------
head(index(Hp))



# ----------
table(index(Hp)$townid, useNA = "always")

table(index(Hp))


