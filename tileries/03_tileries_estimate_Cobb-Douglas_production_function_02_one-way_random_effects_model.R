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
# One-way random effects model
# ------------------------------------------------------------------------------


tile.r <- plm(log(output) ~ log(labor) + log(machine), Tl.p, model = "random")


summary(tile.r)



# -->
# The transformation parameter is now individual specific, more precisely, it depends on the number of available observations for every individual.
# The theta parameter here varies from 0.49 to 0.60



# ----------
plot(tile.r)

