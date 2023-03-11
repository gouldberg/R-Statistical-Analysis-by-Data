setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Landmark registration by manual
# ------------------------------------------------------------------------------
#  This is a MANUAL PGS spurt identification procedure requiring a mouse click at the point where the acceleration curve
#  crosses the zero axis with a negative slope during puberty.
#  Here we do this only for the first 10 children.

children = 1:10

PGSctr = rep(0, length(children))

for (icase in children) {
  accveci = eval.fd(agefine, accelfdUN[icase])
  plot(agefine, accveci,"l", ylim=c(-6,4),
       xlab="Year", ylab="Height Accel.",
       main = paste("Case",icase))
  lines(c(1,18), c(0,0), lty=2)
  PGSctr[icase] = locator(1)$x
  # **** CLICK ON EACH ACCELERATION CURVE WHERE IT
  # **** CROSSES ZERO WITH NEGATIVE SLOPE DURING PUBERTY
}


PGSctr

