setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension



# ------------------------------------------------------------------------------
#  Functional ANOVA:  random projections
#    - Another option is to use the idea of random projections (Cuesta-Alvertos and Febrero-Bande, 2010),
#      which transform the functional data into univariate data,
#      solve the ANOVA problem for this simple situation, and obtain conclusions for the functional data by collecting
#      the information from several projections.
# ------------------------------------------------------------------------------

fdat <- as.data.frame(ftension1$data)

gdat <- as.data.frame(cond)



# ----------
# simple dummy coding with the auditory condition as baseline
( ctrAudio <- contr.treatment(3) )



# ----------
set.seed(222)


# The authors suggest that the number of projections to be used should be min(30, n)
# Since in our data n = 800, we are good with 30 projections

# fitrpm <- anova.RPm(fdat, ~ cond, gdat, RP = 30, contrast = list(cond = ctrAudio))
# summary.anova(fitrpm)

fitrpm <- fanova.RPm(fdat, ~ cond, gdat, RP = 30, contrast = list(cond = ctrAudio))

summary(fitrpm)



# -->
# two-different types of p-value corrections (Bonferroni, false discovery rate), which, in our application, lead to consisten decisions.
# cond:  overall group differences in the sense of an F-test: it suggests that there is a significant overall difference in the means
# C1.cond:  tests visual vs. auditory (significant difference)
# C2.cond:  tests visual vs. auditory-vidual (non significant)



