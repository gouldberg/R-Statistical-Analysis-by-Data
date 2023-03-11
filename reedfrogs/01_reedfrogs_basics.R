setwd("//media//kswada//MyFiles//R//reedfrogs")

packages <- c("dplyr", "rethinking", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  reedfrogs
#   - Experiments exploring Reed frog tadpole mortality.
#   - outcome is "surv"
# ------------------------------------------------------------------------------
data("reedfrogs", package = "rethinking")

d <- reedfrogs

dim(d)

str(d)


# -->
# Note that this is aggregated data, not single-case data



# ------------------------------------------------------------------------------
# histogram and density of surv by density
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))

table(d$density)

histogram(~ surv | density, data = d, breaks = seq(0, 35, 1), type = "density",
          xlab = "surv",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm, col = "black",
                              args = list(mean=mean(x), sd=sd(x)))
          } )


