setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Visualize gait cycle for a single child
# by plotting knee angle against hip angle as time progresses round the cycle.
# ------------------------------------------------------------------------------

# Take mean across boys by each Hip Angle and Knee Angle
( gaitMean = apply(gait, c(1, 3), mean) )

xlim = range(c(gait[,,1], gaitMean[, 1]))

ylim = range(c(gait[,,2], gaitMean[, 2]))



# ----------
# set points to be labelled
( i4 = seq(4, 20, 4) )


# select boys
boys <- c(1,2,3,4)

par(mfrow=c(2,2))

for(i in 1:length(boys)){
  plot(gait[, boys[i], ], type='b', xlim=xlim, ylim=ylim, xlab='Hip angle (degrees)', ylab='Knee angle (degrees)', pch='.', cex=3, lwd=1.5, main = paste0("boy-", boys[i]))
  points(gaitMean, type='b', lty='dotted', pch='.', cex=3, lwd=1.5)
  text(gait[i4, boys[i], ], labels=LETTERS[c(2:5, 1)])
  text(gaitMean[i4, ], labels=LETTERS[c(2:5, 1)])
}


# -->
# The periodic nature of the process implies that this forms a closed curve.
# Also shown for reference purposes is the same relationship for the average across the 39 children.

# Interesting feature in this plot is the cusp occurring at the heel strike as the knee momentarily reverses its extension to absorb the shock.
# The angular velocity is clearly visible in terms of the spacing between numbers, and it varies considerably as the cycle proceeds.

# Here the child is boy1:
# The child whose gait is represented by the solid curve differs from the average in two principal ways.
# First, the portion of the gait pattern in the C-D part of the cycle shows an exaggeration of movement relative to the average.
# Second, in the part of the cyle where the hip is most bent, this bend is markedly less than average;
# interestingly, this is not accompanied by any strong effect on the knee angle.


