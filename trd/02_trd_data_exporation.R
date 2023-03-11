setwd("//media//kswada//MyFiles//R//trd")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  trd (Tokyo rainfall data)
# ------------------------------------------------------------------------------

data("trd", package = "gamlss.data")


str(trd)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


table(trd)


# -->
# only 0, 1, 2 values



par(mfrow = c(1, 1), mar = c(4,4,4,4))

plot(trd, type = "l")

