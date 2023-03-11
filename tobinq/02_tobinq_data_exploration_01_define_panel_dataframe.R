setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")


str(TobinQ)

dim(TobinQ)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


pTobinQ <- pdata.frame(TobinQ)



# ----------
# This is balanced panel
pdim(pTobinQ)



# ----------
head(index(pTobinQ))



# ----------
table(index(pTobinQ)$cusip, useNA = "always")

table(index(pTobinQ)$year, useNA = "always")

table(index(pTobinQ))


