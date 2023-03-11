setwd("//media//kswada//MyFiles//R//engine")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 4. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  engine
#   - some data for 19 Volvo engines
#   - It is often claimed, at least by people with little actual knowledge of engines, that a car engine with a larger cylinder capacity will
#     wear out less quickly than a smaller capacity engine.
# ------------------------------------------------------------------------------
data(engine)
attach(engine)

str(engine)



# ------------------------------------------------------------------------------
# basics visualization
# ------------------------------------------------------------------------------

plot(size, wear, xlab = "Engine capacity", ylab = "Wear index")

