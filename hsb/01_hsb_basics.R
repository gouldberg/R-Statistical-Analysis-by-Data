setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
#   - The data collected as a subset of the High School and Beyond study conducted by the National Education Longitudinal Studies program
#     of the National Center for Education Statistics
#   - Variables
#       - gender, race, socioeconomic status (SES), school type, chosen high school program type
#         scores on reading, writing, math, science, and social studies
#   - We want to determine which factors are related to the choice of the type of program  -- academic, vocational or general that the students pursue in high school
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
