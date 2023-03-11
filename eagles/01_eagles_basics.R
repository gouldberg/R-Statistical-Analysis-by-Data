setwd("//media//kswada//MyFiles//R//eagles")

packages <- c("dplyr", "rethinking", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eagles
#   - Knight and Skagen collcted during a field study on the foraging behaviour of wintering Bald Eagles in Washington State, USA.
#     Data concerning 160 attemps by one (pirating) Bald Eagle to steal a chum salmon from another (feeding) Bald Eagle.
#   - While one eagle feeds, sometimes another will soop in and try to steal the salmon from it. Call the feeding eagle the "victim" and the thief the "pirate".
#   - variables
#       - y:  Number of successful attempts
#       - n:  Total number of attempts
#       - P:  Size of pirating eagle (L=large, S=small)
#       - A:  Age of pirating eagle (I=immature, A=adult)
#       - V:  Size of victim eagle (L=large, S=small)
# ------------------------------------------------------------------------------
data("eagles", package = "MASS")

d <- eagles

dim(d)

str(d)

d



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
eagles.glm <- glm(cbind(y, n - y) ~ P*A + V, data = eagles, family = binomial)

eagles.glm

dropterm(eagles.glm)


# ----------
prof <- profile(eagles.glm)

plot(prof)

pairs(prof)


