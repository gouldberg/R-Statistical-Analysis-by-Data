setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# What extent items constructed to measure the same construct appear homogeneous
# ------------------------------------------------------------------------------

# draw convex hulls around items that belong to the same category

plot(res, main = "", hull.conf = list(hull = TRUE, ind = codes, col = "coral1", lwd = 2))



