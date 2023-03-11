setwd("//media//kswada//MyFiles//R//cpg")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpg
#   - median annual retail price per GB of hard drives taken from a sample of manufacturers from 1980 to 2008.
#   - One of the remarkable technological developments in the computer industry has been the ability to store information densely on a hard drive.
#     In addition, the cost of storage has steadily declined causing problems of too much data as opposed to big data.
# ------------------------------------------------------------------------------

data(cpg, package = "astsa")

str(cpg)

cpg



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

