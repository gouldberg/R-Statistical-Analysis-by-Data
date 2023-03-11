setwd("//media//kswada//MyFiles//R//fabric")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
#  - The data are 32 observations on default in rolls of fabric
#  - variables
#      - leng:  the length of the fabric roll
#      - y:  the number of faults in the roll of the fabric
#      - x:  the log of the length of the roll
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

car::scatterplot(y ~ leng, data = fabric)


car::scatterplot(y ~ x, data = fabric)


