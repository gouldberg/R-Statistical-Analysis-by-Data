setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
library(plm)

jsp.p <- pdata.frame(jsp, index = c("id", "year"))




# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition
summary(jsp.p$math)

summary(jsp.p$english)


# -->
# The variation is mainly due to inter-individual,
# but math variation by inter-individual is a little bit higher than english






