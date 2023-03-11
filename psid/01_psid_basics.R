setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
#   - The Panel Study of Income Dynamics (PSID), begun in 1968, is a longitudinal study of a representative sample of U.S. 
#     individuals described in Hill (1992). The study is conducted at the Survey Research Center, Institute for Social Research,
#     University of Michigan, and is still continuing.
#   - There are currently 8700 households in the study and many variables are measured.
#   - We chose to analyze a random subset of this data, consisting of 85 heads of household who were aged 25-39 in 1968 and had complete data
#     for at least 11 of the years between 1968 and 1990.
#     The variables included were annual income, gender, years of education and age in 1968.
#   - variables:
#       - age:  age in 1968
#       - years:  years of education
#       - income:  annual income in dollars
#       - year:  calendar year
#       - person:  ID number for individual
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

