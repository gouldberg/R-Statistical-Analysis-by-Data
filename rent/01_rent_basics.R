setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
#  - The data come from a survey conducted in April 1993 by Infratest Sozialforschung, where a random sample of accommodation with
#    new tenancy agreements or increases of rents withing the last four years in Munich was selected, including single rooms, small apartments,
#    flats and two-family houses.
#  - main variables:
#       - R:  the response variable which is the monthly net rent in Deutsche Marks (DM), i.e. the monthly rent minus calculated or estimated utility cost
#       - Fl: the floor space in square metres
#       - A:  the year of construction
#       - H:  a 2-level factor indicating whether there is central heating (0 = yes, 1 = no)
#       - loc:  a factor indicating whether the location is below average (1), average (2), or above average (3)
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

PPP <- par(mfrow = c(2,2))
plot(R ~ Fl, data = rent, col = gray(0.7), pch = 15, cex = 0.5)
plot(R ~ A, data = rent, col = gray(0.7), pch = 15, cex = 0.5)
plot(R ~ H, data = rent, col = gray(0.7), pch = 15, cex = 0.5)
plot(R ~ loc, data = rent, col = gray(0.7), pch = 15, cex = 0.5)

par(PPP)



# -->
# Statistical model used for the analysis of the rent data should be able to dela with:
# 1.  Complexity of the relationship between rent and the explanatory variables
# 2.  Non-homogeneity of variance of rent
# 3.  Skewness in the distribution of rent
