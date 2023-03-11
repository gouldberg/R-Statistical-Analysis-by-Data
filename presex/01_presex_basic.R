setwd("//media//kswada//MyFiles//R//presex")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PreSex
#  - A study of divorce patterns in relationto premarital and extramarital sex by Thornes and Collard (1979)
#    These data were analyzed by Agresti (2013) and by Friendly, from which this account draws.
#  - A sample of about 500 people who had petitioned for divorce, and a similar number of married people, were asked two questions
#    regarding their pre- and extramarital sexual experience:
#      - (1) Before you married your (former) husbadn/wife, had you ever made love with anyone else ?
#      - (2) During your (former) marriage (did you) have you had any affiars or brief sexual encounters with another man/woman ?
# ------------------------------------------------------------------------------
data("PreSex", package = "vcd")

str(PreSex)

dim(PreSex)


PreSex



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
