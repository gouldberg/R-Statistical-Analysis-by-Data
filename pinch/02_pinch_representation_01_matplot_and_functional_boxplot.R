setwd("//media//kswada//MyFiles//R//pinch")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pinch
# ------------------------------------------------------------------------------

pinch <- matrix(scan("pinch.txt", 0), 151, 20, byrow=TRUE)

# data("pinch", package = "fda")

head(pinch)



# ------------------------------------------------------------------------------
# Represent by matplot() and its mean, median
# ------------------------------------------------------------------------------

pinch_mean <- apply(pinch, 1, mean)
pinch_median <- apply(pinch, 1, median)



# ----------
graphics.off()
par(mfrow=c(1,1))
matplot(pinch, type = "l", ylab = "Force (N)")
lines(pinch_mean, col = "black", lty = 1, lwd = 2)
lines(pinch_median, col = "black", lty = 2, lwd = 2)



# ------------------------------------------------------------------------------
# Represent by functional boxplot
#   - Functional boxplots are not obtained from pointwise boxplots
#     a measure of centrality for each function compared to the other functions
# ------------------------------------------------------------------------------

fbplot(fit = pinch, ylim = c(-1, 11), ylab = "seconds", ylab = "Force (N)")


# -->
# Black line is the median curve.
# The magenta region represents the middle 50% of curves, and the dashed red curves are the outlying functions
# Note that functional boxplots produce outlying curves rather than individual outlying.



