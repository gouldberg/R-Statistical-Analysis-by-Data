setwd("//media//kswada//MyFiles//R//neuralactivity")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  NeuralActivity
# ------------------------------------------------------------------------------

data("NeuralActivity", package = "MPsychoR")

str(NeuralActivity)

length(NeuralActivity)

dim(NeuralActivity[[1]])



NeuralActivity



# ------------------------------------------------------------------------------
# basic analysis:  correlation matrix
# ------------------------------------------------------------------------------

NeuralActivity[[1]]

rownames(NeuralActivity[[1]])



# ----------
library(corrplot)


# Note that this is not correlation matrix, but dissimilarity matrix, but we try.
corrplot(NeuralActivity[[1]], method = "shade", order = "hclust", hclust.method = "ward.D2", type = "lower", is.corr = TRUE)


# -->
# almost no cluster ...





