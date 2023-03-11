setwd("//media//kswada//MyFiles//R//morse")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  morse
#   - Rothkopf (1957) studied to what extent 598 test persons confused different acoustic Morse signals.
#     He used 36 different signals, the 26 letters of the alphabet, and the natural numbers from 0 to 9.
#     In the experiment, each person had to judge whether two signals, i and j, presented acoustically one after the other,
#     were the same or not the same. Both (i, j) and (j, i) had to be judged in the experiment.
#   - The percentage of "Same !" judgments (i.e., the confusion probability) for each pair
#     is taken as a measure of the psychological similarity of each pair.
# ------------------------------------------------------------------------------

# morse2 = 1 - confusion probabilities
data(morse, package = "smacof")
data(morse2, package = "smacof")


str(morse)
str(morse2)


morse
morse2



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


