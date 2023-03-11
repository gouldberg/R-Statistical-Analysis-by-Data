setwd("//media//kswada//MyFiles//R//neuralactivity")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  NeuralActivity
#   - dataset from the cognitive neuroscience area and based on fMRI scans.
#     Tamir et al. (2016) scanned 20 participants while performing a task designed to elicit their thoughts about 60 mental states.
#     On each trial, participants saw the name of a mental state (e.g., "awe") and decided which of two scenarios would better evoke
#     that mental state in another person (e.g., "seeing the Pyramids" or "watching a meteor shower").
#     Based on these measures, they derived a 60 * 60 correlation matrix for each state, subsequently converted into a dissimilarity matrix.
#     In total, we have 20 such dissimilarity matrices, one for each participant.
# ------------------------------------------------------------------------------

data("NeuralActivity", package = "MPsychoR")

str(NeuralActivity)

length(NeuralActivity)

dim(NeuralActivity[[1]])



NeuralActivity




