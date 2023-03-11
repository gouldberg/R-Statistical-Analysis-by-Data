setwd("//media//kswada//MyFiles//R//employment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Employment status
#  - Data from a 1974 Danish study of 1,314 employees who had been laid off are given in the data table Employment in vcd.
#  - The workers are classified by
#      - (a) their employment status, on January 1, 1975  ("NewJob" or still "Unemployed")
#      - (b) the length of their employment at the time of layoff
#      - (c) the cause of their layoff ("Closure", etc., or "Replaced")
# ------------------------------------------------------------------------------
data("Employment", package = "vcd")

data <- Employment

data



# ------------------------------------------------------------------------------
# structable
# ------------------------------------------------------------------------------
structable(data)

