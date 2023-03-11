setwd("//media//kswada//MyFiles//R//math_achieve")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathAchieve
#  - Example from Raudenbush and Bryk's influential text on hierarchical linear models (2002) and also appears in a paper by Singer (1998),
#    who demonstrates how such models can be fit by the MIXEE procedure in SAS.
#  - The data from the 1982 "High School and Byyond" (HSB) survey, are for 7,185 high school students from 160 schools.
#    There are on average 7,185 / 160 = 45 students per school.
#    We have two levels of hierarchy. with schools at the higher or group level and strudents within schools as the lower or
#    individual level.
#  - The second data set pertains to the schools into which students are clustered, and there is one row for each of the M = 160 schools
#  - Variabels:
#      - SES:  the socioeconomic status of each student's family. The scores are centered to have a mean of zero,
#              within rounding error, for the 7185 students in the sample.
#      - MathAch:  the student's score on a math achievement test, another student-level variable, and the response variable in the models
#                  that we consider below.
#      - MEANSES:  mean SES for the school, however do not correspond to the sample means we compute from the student data in the MathAchieve data frame.
# ------------------------------------------------------------------------------

data("MathAchieve", package = "nlme")

dim(MathAchieve)

str(MathAchieve)

car::some(MathAchieve)



# ----------
data("MathAchSchool", package = "nlme")

dim(MathAchSchool)

str(MathAchSchool)

car::some(MathAchSchool)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
