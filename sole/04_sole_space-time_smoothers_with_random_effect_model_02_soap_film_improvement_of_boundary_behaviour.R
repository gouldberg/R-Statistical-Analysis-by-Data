setwd("//media//kswada//MyFiles//R//sole")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sole
# ------------------------------------------------------------------------------

data("sole", package = "gamair")

str(sole)

head(sole)



# ------------------------------------------------------------------------------
# Soap film improvement of boundary behaviour
# ------------------------------------------------------------------------------

sole$station <- solr$station



# ----------
# Tensor product smooths with time
#   - "sf":  soal film  
#   - "sw":  soap wiggly
# we want the boundary smooth to be constant in space and time, we can drop the tensor product with "sf" altogether.

# knots of the soap film smoother:  I simply selected a nicely spread out subset of the sampling station locations for one sampling occasion.

# xt$bnd:  the boundary specification for the smooth
# To obtain a suitable boundary polygon for the sole egg data, need to produce a plot of the sampling station locations and coast,
# and then use R's locator function to define a suitable simple boundary ...

# NULL:  the second item can be NULL as the "cr" basis does not require an xt


# NOT RUN:  need to locate xt$bnd ...
som2 <- bam(eggs ~ te(lo, la, t, bs = c("sw", "cr"), k = c(40, 5), d = c(2, 1), xt = list(list(bnd = bnd), NULL)) +
              s(t, k = 5, by = a) + offset(off) + s(station, bs = "re"),
            knots = knots, family = quasipoisson, data = sole)


