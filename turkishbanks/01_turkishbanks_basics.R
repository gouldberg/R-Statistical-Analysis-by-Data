setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Turkish Banks
#  - This data were used by Sl-Gamal and Inanoglu (2005) to analyze production costs of banks
# ------------------------------------------------------------------------------

data("TurkishBanks", package = "pder")


str(TurkishBanks)


dim(TurkishBanks)


car::some(TurkishBanks)

  

# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


