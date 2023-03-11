setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


Fa <- pdata.frame(Fatalities)



# ----------
# This is balanced panel
pdim(Fa)



# ----------
head(index(Fa))



# ----------
table(index(Fa)$state, useNA = "always")

table(index(Fa)$year, useNA = "always")

table(index(Fa))


