setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlation matrix
# ------------------------------------------------------------------------------

library(smacof)


cordist <- psych::cor2dist(cor(Rogers))


fit <- mds(cordist, type = "interval")

fit


# ----------
par(mfrow = c(1, 1))
plot(fit)

