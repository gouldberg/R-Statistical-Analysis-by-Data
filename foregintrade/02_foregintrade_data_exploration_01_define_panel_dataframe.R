setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Foreign Trade
# ------------------------------------------------------------------------------

data("ForeignTrade", package = "pder")


str(ForeignTrade)


dim(ForeignTrade)


car::some(ForeignTrade)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

FT <- pdata.frame(ForeignTrade)



# ----------
# This is balanced panel
pdim(FT)



# ----------
head(index(FT))



# ----------
table(index(FT)$country, useNA = "always")

table(index(FT)$year, useNA = "always")

table(index(FT))


