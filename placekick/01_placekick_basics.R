setwd("//media//kswada//MyFiles//R//placekick")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  placekick
#   - In American and Canadian football, points can be scored by kicking a ball through a target area (goal) at each end of the field.
#     A success occurs when the football is kicked over the crossbar and between the two uprights of the goal posts.
#     The placekicker's team receives 1 or 3 points, where a point after touchdown (PAT) receives 1 point and a field goal receives 3 points.
#     A placekick that is not successful receives 0 points.
#   - Bilder and Loughlin (1998) use logistic regression to estimate the probability of success for a placekick (PAT or field goal) in the
#     National Football League (NFL).
#     They examine a number of explanatory variables, including:
#       - week:  Week of the season
#       - distance:  Distance of the placekick in yards
#       - change:  Binary variable denoting lead-change (1) vs. non-lead-change (0) place kicks;
#         lead-changing placekicks are those that have the potential to change which team is winning the game
#       - elap30:  Number of minutes remaining before the end of the half, with overtime placekicks receiving a value of zero
#       - PAT:  Binary variable denoting the type of placekick, where a PAT attempt is a 1 and a field goal attempt is a zero
#       - type:  Binary variable denoting outdoor (1) vs. dome (0) placekicks
#       - field:  Binary variable denoting grass (1) vs. artificial turf (0) placekicks
#       - wind:  Binary variable for placekicks attempted in windy conditions (1) vs. non-windy conditions (0); 
#         we define windy as a wind stronger than 15 miles per hour at kickoff in an outdoor stadium
#       - good:  Binary variable denoting successful (1) vs. failed (0) placekicks;  this is one response variable.
#   - There are 1425 placekick observations collected during the 1995 NFL season that are available in the data set.
# ------------------------------------------------------------------------------

placekick <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter2//Placekick.csv")

str(placekick)

car::some(placekick)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

Hmisc::describe(placekick)
