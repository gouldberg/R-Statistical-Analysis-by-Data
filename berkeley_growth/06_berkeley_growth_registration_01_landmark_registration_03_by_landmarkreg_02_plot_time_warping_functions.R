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
# Plot warping functions for cases 3 and 7
# ------------------------------------------------------------------------------

ageRng  = range(age)

nfine   = 101

agefine = seq(ageRng[1], ageRng[2], length=nfine)

warpfdLM  = regListLM$warpfd

warpmatLM = eval.fd(agefine, warpfdLM)


# ----------
op = par(mfrow=c(2,2), ask=FALSE)
plot(accelfdUN[3], xlim=c(1,18), ylim=c(-3,2.0), lty=1, lwd=2, xlab="", ylab="")
lines(c(PGSctrmean, PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)

plot(agefine, warpmatLM[,3], "l", lty=1, lwd=2, col=1, cex=1.2, xlab="", ylab="")
lines(agefine,  agefine, lty=2, lwd=1.5)
lines(c(PGSctrmean, PGSctrmean), c(1,18), lty=2, lwd=1.5)
text(PGSctrmean+0.1, warpmatLM[61,3]+0.3, "o", lwd=2)


plot(accelfdUN[7], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=2, xlab="", ylab="")
lines(c(PGSctrmean, PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)

plot(agefine, warpmatLM[,7], "l", lty=1, lwd=2, col=1, cex=1.2, xlab="", ylab="")
lines(c(PGSctrmean, PGSctrmean), c(1,18), lty=2, lwd=1.5)
lines(agefine, agefine, lty=2, lwd=1.5)
text(PGSctrmean+0.1, warpmatLM[61,7]+0.2, "o", lwd=2)

par(op)




# ----------
# The top panel:
#  - The growth acceleration curve on the left and the corresponding time-warping function h(t) on the right for the girl among the first 10
#    in the Berkeley growth study with the earliest pubertal growth spurt.
# The bottom panel
#  - The corresponding plots for the girl with the latest growth spurt.
# The middle of the growth spurt is shown as the vertical dashed line in all panels


