setwd("//media//kswada//MyFiles//R//indvalues")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  indvalues
# ------------------------------------------------------------------------------

data("indvalues", package = "smacof")

str(indvalues)



# ------------------------------------------------------------------------------
# Circular unfolding
#   - Fit a spherically restricted unfolding model which forces the column configuration to be on a circle
# ------------------------------------------------------------------------------

library(smacof)

fitSchwartz_circle <- unfolding(indvalues, type = "interval", circle = "column")

fitSchwartz_circle



# -->
# We get a stress-1 value of 0.178, a little bit worse than previous model (= 0.171)



# ------------------------------------------------------------------------------
# Row-conditional unfolding
#   - It abandons the assumption of homogeneous or consistent response scores across all individuals.
#     It is known from the literature that there are cultural scoring differences, especially when it comes to extreme responses.
#     In such cases, row-conditional unforlding can be applied.
# ------------------------------------------------------------------------------

library(smacof)


# IT TAKES TIME: 
fitSchwartz_rowcon <- unfolding(indvalues, type = "interval", conditionality = "row")

fitSchwartz_rowcon



# -->
# We get a stress-1 value of 0.176, a little bit worse than previous model (= 0.171)



# ----------
# Joint configuration plot

# Here we are not really interested in interpreting associations among individuals.
# Rather, we focus on the value scores and see whether they are approximately arranged on a circle.

graphics.off()
par(mfrow = c(1,3))

plot(fitSchwartz, label.conf.rows = list(label = FALSE))
plot(fitSchwartz_circle, label.conf.rows = list(label = FALSE))
plot(fitSchwartz_rowcon, label.conf.rows = list(label = FALSE))


