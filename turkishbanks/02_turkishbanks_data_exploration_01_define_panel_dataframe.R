setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Turkish Banks
# ------------------------------------------------------------------------------

data("TurkishBanks", package = "pder")


str(TurkishBanks)


dim(TurkishBanks)


car::some(TurkishBanks)



# ----------
summary(TurkishBanks)
  


# -->
# Many NA's ... omit NAs

TurkishBanks <- na.omit(TurkishBanks)

summary(TurkishBanks)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

TB <- pdata.frame(TurkishBanks)



# ----------
# This is unbalanced panel
pdim(TB)



# ----------
head(index(TB))



# ----------
table(index(TB)$id, useNA = "always")

table(index(TB)$year, useNA = "always")

table(index(TB))


