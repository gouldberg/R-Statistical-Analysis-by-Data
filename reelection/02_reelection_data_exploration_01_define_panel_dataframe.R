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
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


Rp <- pdata.frame(Reelection)



# ----------
# This is unbalanced panel
pdim(Rp)



# ----------
head(index(Rp))



# ----------
table(index(Rp)$country, useNA = "always")

table(index(Rp)$year, useNA = "always")

table(index(Rp))


