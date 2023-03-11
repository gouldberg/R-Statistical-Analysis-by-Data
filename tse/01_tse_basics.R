setwd("//media//kswada//MyFiles//R//tse")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tse
#   - The data are for the eleven-year period 1 January 1988 to 31 December 1998. Continuously compounded returns in domestic currency
#     were calculated as the first difference of the natural logarithm of the series.
#   - Variables:
#        - year, month, day
#        - ret:  day return ret[t] = ln(currency[t]) - ln(currency[t-1])
#        - currency:  the currency exchange rate
#        - tl:  day return ret[t] = log10(currency[t] - log10(currency[t-1])
# ------------------------------------------------------------------------------

data("tse", package = "gamlss.data")


str(tse)

car::some(tse)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
