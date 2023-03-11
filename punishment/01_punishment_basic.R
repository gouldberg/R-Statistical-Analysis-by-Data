setwd("//media//kswada//MyFiles//R//punishment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Punishment
#  - contains the result of a study by the Gallup Institute in Denmark in 1979 about the attitude of a random sample of 1,456 persons towards
#    corporal punishment of children (andersen, 1991).
#  - This dataset is a frequency data frame representing a 2 * 2 * 3 * 3 table, with table variables
#      - (a) attitude toward use of corporal punishment (approve of "moderate" use or "no" approval)
#      - (b) memory of whether the respondent had experienced corporal punishment as a child (yes/no)
#      - (c) education level of respondent (elementary, secondary, high), and
#      - (d) age category of respondent
# ------------------------------------------------------------------------------

data("Punishment", package = "vcd")


data <- Punishment


data


str(Punishment)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
