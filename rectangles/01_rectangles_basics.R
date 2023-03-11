setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
#   - Data for experiment by Borg and Leutner (1983).
#     They constructed rectangles on the basis of the grid design. Each point in this grid defines a rectangle.
#     A total of 21 persons rated (twice) the similarity of each pair of these 16 rectangles on av 10-point scale ranging from "0 = equal, identical"
#     to "9 = very different".
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


