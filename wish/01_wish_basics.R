setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
#   - Wish (1971) wanted to know the attributes that people use when judging the similarity of different countries
#     He conducted an experiment where 18 students were asked to rate each pair of 12 different countries on their overall similarity.
#     For these ratings, an answer scale from "extremely dissimilar" (coded as 1) to "extremely similar" (coded as 9) was
#     offered to the respondents. No explanation was given on what was meant by "similar".
#   - The observed similarity ratings, averaged over the 18 respondents.
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

