setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
#  - Data from the Junior School Project collected from primary (U.S. term is elementary) schools in inner London.
#    The data is described in detail in Mortimore et al. (1988) and a subset is analyzed extensively in Goldstein (1995).
#  - The variables in the data are
#       - school
#       - class within the school (up to four)
#       - gender
#       - social class of the father (I=1; I=2; III nonmanual=3; III manual=4; IV=5, V=6; Long-term unemployed=7; Not currently employed=8; Father absent=9)
#       - raven's test in year 1
#       - student id number
#       - english test score
#       - mathematics test score
#       - school year (coded 0, 1 and 2 for years one, two and three)
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
