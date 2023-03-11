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
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(log(income) ~ as.numeric(year) + sex + as.numeric(age) + as.numeric(educ), ps.p)



# -->
# But here for the error, the share of the individual effect and that of the idiosyncratic effect are 34.4% and 65.6%, respectively

# theta = 0.717 (at median):  GLS estimator removes about 71.7% of the individual mean

