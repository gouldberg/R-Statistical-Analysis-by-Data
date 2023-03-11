setwd("//media//kswada//MyFiles//R//friedman1")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "mlbench")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: friedman1
#   - Friedman (1991) introduced several benchamark data sets create by simulation.
#     One of these simulations used the following nonlinear equation to create data:
#     y = 10 * sin(pi * x1 * x2) + 20(x3 - 0.5)^2 + 10 * x4 + 5 * x5 + N(0, sigma^2)
#     where the x values are random variables uniformly distributed between [0,1]
#     (there are also 5 other non-informative variables also created in the simulation)
#   - The mlbench contains a function called mlbench::friedman1 that simulates these data
# ------------------------------------------------------------------------------
set.seed(200)

trainData <- mlbench.friedman1(200, sd = 1)

dim(trainData$x)

trainData$y


# ----------
# convert matrix to dafa frame, this will give the columns names
trainData$x <- data.frame(trainData$x)



# ----------
featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))
