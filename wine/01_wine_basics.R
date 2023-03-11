setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - Characteristics extracted from wine tasting by 12 professionals, of a set of 10 dry white wines from Touraine:
#     five Touraine Protected Appellation of Origin (AOC) from Sauvignon vines, and five Vourvray AOC from Chenin vines in the Loire Valley.
#     This is within the context of research into the characteristic of the wine from Chenin vines in the Loire Valley.
#     These wines were chosen with the objective of illustrating the diversity of Loire Valley wines within each specific vine.
#     It must be noted that there is very little connection between the Protected Appellation of Origin (AOC) and the vine (in terms of experimental planning)
#     Participants were given a questionnaire comprising 10 open questions (one per wine)
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


# Here we find very near to the threshold for CA use, as the total number of occurrences (n = 245) is extremely low.
# Nonetheless, the analysis is possible as wine experts tend to use rather standardised vocabulary, thus yielding a low total number of words
# and thus a "sufficient" number of words with an acceptable frequency.

# In this type of analysis ,we eliminate the words which are used the least frequently.
# Due to the small sample size, the threshold below which words were no longer included was set at 4.
# This threshold was determined empirically: with this data, setting the threshold at 5 would not change the representation significantly,
# but would remove important wors (such as "Sauvignon").
# On the other hand, a threshold of 3 would lead to heavily laden graphs and words with weak coordinates.

