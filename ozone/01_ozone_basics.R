setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
#   - data from a study of the relationship between atmospheric ozone concentration and other meteorological variables
#     in the Los Angeles Basin in 1976.
#     A number of cases with missing variables have been removed for simplicity.
#     The data were first presented by Breiman and Friedman (1985).
#   - Variables
#       - O3: the atmospheric ozone concentratoin on a particular day
#       - temp:  temperature measured at El Monte
#       - ibh:  inversion base height at LAX
#       - ibt:  inversion top temperature at LAX
# ------------------------------------------------------------------------------

# data(ozone, package="faraway")

ozone <- read.csv(file = "ozone.txt", header = T, sep = "\t")


str(ozone)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

