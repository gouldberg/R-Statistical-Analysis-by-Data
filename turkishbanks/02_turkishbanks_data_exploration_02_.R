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



# ----------
TB <- pdata.frame(TurkishBanks)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

