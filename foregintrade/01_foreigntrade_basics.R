setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Foreign Trade
#  - Kinai and Lahiri (1993) studied the determinants of international trade for developing countries and especially the measure of the price and income elasticities.
#    This question is very important because it crucially determines the growth and debt of these countries.
#    The panel dataset used concerns 31 developing countries, for the period 1964 - 1986.
# ------------------------------------------------------------------------------

data("ForeignTrade", package = "pder")


str(ForeignTrade)


dim(ForeignTrade)


car::some(ForeignTrade)

  

# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


