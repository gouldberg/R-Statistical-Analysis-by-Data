setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ------------------------------------------------------------------------------
# data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(chredlin)


psych::describe(chredlin)



# -->
# We see that there is a wide range in the race variable, with some zip codes almost entirely minority or non-minority.
# This is good for our analysis since it will reduce the variation in the regression coefficient for race, allowing us to assess this effect
# more accurately.

# We also note some skewness in the theft and income variables.




# ------------------------------------------------------------------------------
# linear regression by race
# ------------------------------------------------------------------------------

# race:  racial composition in percentage of minority

summary(lm(involact ~ race, data = chredlin))



# -->
# We can clearly see that homeowners in zip codes with a high percentage of minorities are taking the default FAIR plan insurance at a higher rate
# than other zip codes.

# However, can the insurance companies claim that the discrepancy is due to greater risks in some zip codes ?
# The insurance companies could claim that they were denying insurance in neighborhoods
# where they had sustained large fire-related losses and
# any discriminatory effect was a by-product of legitimate business practice.

