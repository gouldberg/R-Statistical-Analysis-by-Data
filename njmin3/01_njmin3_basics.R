setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  njmin3
#   - dataset to estimate the effect of change of minimum wage law change to full-time equivalent employment  (minimum wage is increased)
#   - Variables
#        - fte:  full-time equivalent employment (response variable)
#        - d:  "after" dummy, with d = 1 for the after period and d = 0 for the before period
#        - nj:  dummy that marks the treatment group
#               (nj = 1 if unit (fast-food restaurants) i is in New Jersey, where the minimum wage law has been changed, 
#               and nj = 0 if unit i in Pennsylvania, where the minimum wage law has not changed)
#               In other words, units (fast-food restaurants) located in New Jersey form the treatment group, and units located in Pennsylvania form the control group
#        - demp:  after-minus-before difference in employment
# ------------------------------------------------------------------------------

data("njmin3", package = "POE5Rdata")

dim(njmin3)

str(njmin3)


car::some(njmin3)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
