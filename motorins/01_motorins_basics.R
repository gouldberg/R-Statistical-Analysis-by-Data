setwd("//media//kswada//MyFiles//R//motorins")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  motorins
#   - In Hallin and Ingenbleek (1983) data on payments for insurance claims for various areas of Sweden in 1977 are presented.
#     The data is further subdivided by mileage driven, the bonus from not having made precious claims and the type of car.
# ------------------------------------------------------------------------------

data(motorins, package="faraway")

str(motorins)

car::some(motorins)



# ------------------------------------------------------------------------------
# histogram of resist
# ------------------------------------------------------------------------------

summary(motorins)

hist(motorins$Payment)

