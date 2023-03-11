setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
#   - The results of decathlon events during two athletic meetings which took place one month apart in 2004:
#     the Olympic Games in Athens which took place on 23 and 24 August, and the Decastar 2004 which took place on 25 and 26 September.
#     For both competitions, the followin information is available for each athlete: performance for each of the 10 events, total number of points
#     (for each event, an athlete earns joints based on performance; here the sum of points scored), and final ranking.
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
