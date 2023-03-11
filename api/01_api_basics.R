setwd("//media//kswada//MyFiles//R//api")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  subsample from the California Academic Performance Index
#   - subsample from the California Academic Performance Index, an annual set of tests used to evaluate California schools.
#   - The API website, including the originaldata files are athttp://api.cde.ca.gov.
#     The subsample was generated as ateaching example by Academic Technology Services at UCLA and
#     was obtainedfromhttp://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm.
#   - We have a cluster sample (apiclus1) in which 15 school districts were sampled and then all schools in each district.
#     The two-stage sample is defined by the sampling unit (dnum) and the population size(fpc).
# ------------------------------------------------------------------------------

data(api)

str(apiclus1)



