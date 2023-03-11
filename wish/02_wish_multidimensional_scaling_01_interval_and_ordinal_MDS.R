setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

# "wish" data is recorded as averaged similarity ranging from 1 to 9, so we convert and scaling to disimilarity ranging from 0 to 1
library(smacof)

fit <- mds((9 - wish) / 8, type = "interval")

fit


# ----------
par(mfrow = c(1, 1))
plot(fit)



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by co-occurence
# ------------------------------------------------------------------------------

# method = 7 is converting "cooccurence" similarities into dissimilarities

( diss <- sim2diss(wish, method = 7) )

res <- mds(diss, type = "ordinal")

plot(res)



