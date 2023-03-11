setwd("//media//kswada//MyFiles//R//indvalues")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  indvalues
# ------------------------------------------------------------------------------

data("indvalues", package = "smacof")

str(indvalues)



# ------------------------------------------------------------------------------
# Unfolding
#   - In SMACOF slang, unfolding is called rectangular SMACOF.
# ------------------------------------------------------------------------------

library(smacof)

fitSchwartz <- unfolding(indvalues, type = "interval")

fitSchwartz



# -->
# We get a stress-1 value of 0.171.



# ------------------------------------------------------------------------------
# Joint configuration plot
#   - Plot the row and column scores into the same space.  The distances between each pair of points can be interpreted.
# ------------------------------------------------------------------------------

# Here we are not really interested in interpreting associations among individuals.
# Rather, we focus on the value scores and see whether they are approximately arranged on a circle.

graphics.off()
par(mfrow = c(1,1))

plot(fitSchwartz, label.conf.rows = list(label = FALSE))


# Also includes a least-squares-based circle fit
circ <- fitCircle(fitSchwartz$conf.col[,1], fitSchwartz$conf.col[,2])
draw.circle(circ$cx, circ$cy, circ$radius, border = "gray", lty = 2)



# -->
# Borg et al. (2017) came to the conclusion that the value circle also exists within persons.


