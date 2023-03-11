setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------

data("TV", package = "vcdExtra")


TV

dim(TV)

str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2




# ------------------------------------------------------------------------------
# 2-way and larger tables:  classical test of independence
# First, the independence of Day and Network is equivalent to the model [Day] [Network]
# ------------------------------------------------------------------------------


DN_M <- MASS::loglm(~ Day + Network, data = TV2)



DN_M



# -->
# The output includes both the chi-square statistic and the deviance test statistics, 
# both significant, indicating strong lack of fit.


