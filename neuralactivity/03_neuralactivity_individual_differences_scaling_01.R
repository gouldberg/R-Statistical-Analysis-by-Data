setwd("//media//kswada//MyFiles//R//neuralactivity")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  NeuralActivity
# ------------------------------------------------------------------------------

data("NeuralActivity", package = "MPsychoR")

str(NeuralActivity)


NeuralActivity



# ------------------------------------------------------------------------------
# Individual Differences Scaling (three-way MDS)
#  --> INDSCAL model (The most prominent 3-way MDS model)
#   - INDSCAL assigns a different weight to each individual. Formally, it uses a weighted Euclidean distance on the individual configurations
#   - Individual spaces can be generated from a single group stimulus space by appropriate weighting of the dimensions.
#   - INDSCAL assumes common dimensions for each individual.
#     It implies that the dimensions are fixed and the group stimulus space should not be subject to rotations.
# ------------------------------------------------------------------------------

# fit a interval INDSCAL using smacof on a subset of these data (10 individuals only) since it takes a while to estimate the model on the full dataset.
NeuralActivity[1:10]

library(smacof)

fitNeuro <- indscal(NeuralActivity[1:10], type = "interval", itmax = 5000)

fitNeuro



# -->
# The stress is 0.387.



# ----------
# Individual configurations
fitNeuro$conf



# ----------
plot(fitNeuro)



