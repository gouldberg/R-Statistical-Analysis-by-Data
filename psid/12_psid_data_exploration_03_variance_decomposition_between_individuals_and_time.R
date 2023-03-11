setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")

str(psid)

car::some(psid)



# ----------
library(plm)

ps.p <- pdata.frame(psid, index = c("person", "year"))



# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition
summary(log(ps.p$income))



# -->
# The variation is mainly due to inter-individual






