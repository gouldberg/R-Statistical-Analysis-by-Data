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



# ----------
FT <- pdata.frame(ForeignTrade)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

