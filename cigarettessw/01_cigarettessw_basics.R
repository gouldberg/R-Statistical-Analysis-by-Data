setwd("//media//kswada//MyFiles//R//cigarettessw")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CigarettesSW
#    - a panel data set that contains observations on cigarette consumption and several economic indicators for all 48 continental federal states
#      of the U.S. from 1985 to 1995
#
#    - We are interested in estimating beta1 in log(Q) = beta0 + beta1 * log(P) + u
#      where Q is the number of cigarette packs per capita sold and P is the after-tax average real price per pack of cigarettes in state i
#      The instrumental variable we are going to use for instrumenting the endogenous regressor log(P) is SalesTax, 
#      the portion of taxes on cigarettes arising from the general sales tax. SalesTax is measured in dollars per pack.
#
#    - The idea is that SalesTax is a relevant instrument as it is included in the after-tax average price per pack.
#      Also, it is plausible that SalesTax is exogenous since the sales tax does not influence quantity sold directly
#      but indirectly through the price.
#
#    - variables:
#         - cpi:  Consumer price index
#         - population:  State population
#         - packs:  Number of packs per capita
#         - income:  State personal income (total, nominal)
#         - tax:  Average state, federal and average local excise taxes for fiscal year
#         - price: Average price during fiscal year, including sales tax
#         - taxs:  Average excise taxes for fiscal year, including sales tax
# ------------------------------------------------------------------------------


data("CigarettesSW", package = "AER")



dim(CigarettesSW)


str(CigarettesSW)



car::some(CigarettesSW)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


