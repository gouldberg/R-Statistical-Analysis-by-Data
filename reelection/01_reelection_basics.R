setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
#   - Brender and Drazen (2008) studied the influence of fiscal policy on the reelection of politicians.
#     It is often suggested that, just before elections, politicians implement more expansionary fiscal policies,
#     i.e., they reduce taxes or increase public spending.
#     A panel of 75 countries is used, with a number of observations varying from 1 to 16.
#   - A subsample of these data is also considered when the incumbent is a candidate to the next election (for the other observations, 
#     reelection means that the incumbent political party wins the election).
#     This subsample can be selected using the dummy variable narrow.
#   - Variables:
#        - "narrow":  A subsample of these data is also considered when the incumbent is a candidate to the next election (for the other observations, 
#          reelection means that the incumbent political party wins the election).
#          This subsample can be selected using the dummy variable narrow.
#        - "reelect":  it equals 1 in case of reelection and 0 otherwise.
#        - 2 main covariates are "ddefterm" and "ddefey":  Both measure the change in the ratio of government balance (budget surplus) and GDP.
#          The first one is the difference between the two years prior to the elections and the two previous years.
#          For the second, this is the difference between the election year and the previous year.
#        - Control variables include the growth rate of GDP "gdppc" and dummies for developting countries "dev",
#          for new democracies and for majoritarian electoral systems "maj".
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


