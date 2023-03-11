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
# Separate Before and After
#   - This is angle data, so After (1.0 - 1.225) = Before (-0.225 - 0)
# ------------------------------------------------------------------------------

# add 0 and 1 at the both ends
Time = c(0, as.numeric(dimnames(gait)[[1]]), 1)

Time



# ----------
# mean of time 1(0.025) and 20(0.975)
gait01        = 0.5 * (gait[20,,] + gait[1,,])


Gait          = array(NA,  dim=c(22, 39, 2))

dimnames(Gait)= c(list(Time), dimnames(gait)[2:3])

Gait


# time 0.000, 0.025 - 0.975 and 1
# mean value for 0.000 and 1.000
Gait[1,,]     = gait01
Gait[2:21,,]  = gait
Gait[22,,]    = gait01

Gait



# ----------
# Define before and after
before = 17:22
Before = c(seq(-0.225, -0.025, by=0.05), 0)

after = 1:6
After = c(1, seq(1.025, 1.225, by=0.05))


Before
After


# ----------
Gait[before,,'Hip Angle']
Gait[before,,'Knee Angle']

Gait[after,,'Hip Angle']
Gait[after,,'Knee Angle']



# ------------------------------------------------------------------------------
# plot values by matlines()
#   - This is angle data, so After (1.0 - 1.225) = Before (-0.225 - 0)
# ------------------------------------------------------------------------------

op = par(mfrow=c(2,1), mar=c(4, 4, 1, 2) + .1)

matplot(Time, Gait[,,'Hip Angle'], ylab='Hip Angle (degrees)', type='l', xlim=c(-0.25, 1.25), lty='solid', xlab='')
matlines(Before, Gait[before,,'Hip Angle'], lty='dotted')
matlines(After, Gait[after,,'Hip Angle'], lty='dotted')
abline(v=0:1, lty='dotted')


matplot(Time, Gait[,,'Knee Angle'], ylab='Knee Angle (degrees)', type='l', xlim=c(-0.25, 1.25), lty='solid', xlab='Time (portion of gait cycle)')
matlines(Before, Gait[before,,'Knee Angle'], lty='dotted')
matlines(After, gait[after,,'Knee Angle'], lty='dotted')
abline(v=0:1, lty='dotted')
par(op)



# -->
# The angles in the sagittal plane formed by the hip and knee as 39 children go through a gait cycle.
# The interval [0, 1] is a single cycle, and the dotted curves show the periodic extension of the data beyond either end of the cycle.
#
# We see that the knee shows a two-phase process, while the hip motion is single-phase.
# It is harder to see how the two joints interact: The figure does not indicate which hip curve is paired with which knee curve.

