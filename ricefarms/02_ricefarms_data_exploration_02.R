setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
#  - dataset from Stock and Watson (2007), data are 1982 to 1988 for each of the continental US states.
#  - The research question if whether taxing alcoholics can reduce the road's death toll.
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


car::scatterplot(frate ~ beertax, data = Fatalities)

