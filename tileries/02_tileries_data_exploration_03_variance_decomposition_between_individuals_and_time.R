setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)



# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition:  "output" is dependent variable in this example
summary(log(Tl.p$output))


summary(log(Tl.p$labor))


summary(log(Tl.p$machine))



# -->
# The variation is mainly due to inter-individual, but only 51.7%
# This data is actually NOT heterogeneity data.





