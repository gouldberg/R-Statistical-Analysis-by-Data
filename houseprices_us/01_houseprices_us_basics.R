setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
#  - Holly et al. (2010) analyze changes in real house prices in 49 US states between 1975 and 2003 to assess to which extent they are
#    driven by fundamentals like disposable per capita income, net borrowing costs, and population growth.
#    The hypothesis of interest is whether house prices have an income elasticity of one.
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


