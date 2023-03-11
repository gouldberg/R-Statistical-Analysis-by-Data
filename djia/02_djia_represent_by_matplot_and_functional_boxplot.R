setwd("//media//kswada//MyFiles//R//djia")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Dow Jones Industrial Average
# ------------------------------------------------------------------------------

djia <- read.csv("Dow_companies_data.csv")


str(djia)


head(djia)



# ------------------------------------------------------------------------------
# Represent cumulative returns on all stocks by matplot() and its mean, median
# ------------------------------------------------------------------------------

# Create a 252 by 30 matrix that contains the cumulative returns on all stocks in DJIA
cum_return <- function(x) 100 * ( x - x[1] ) / x[1]

cr <- apply(djia[,2:31], 2, FUN = cum_return)

cr



# ----------
cr_mean <- apply(cr, 1, mean)
cr_median <- apply(cr, 1, median)



# ----------
graphics.off()
par(mfrow=c(1,1))
matplot(cr, type = "l")
lines(cr_mean, col = "black", lty = 1, lwd = 2)
lines(cr_median, col = "black", lty = 3, lwd = 2)



# ------------------------------------------------------------------------------
# Represent by functional boxplot
#   - Functional boxplots are not obtained from pointwise boxplots for each day
#     a measure of centrality for each function compared to the other functions is used
# ------------------------------------------------------------------------------

fbplot(fit = cr, ylim = c(-20, 90), xlab = "Trading day", ylab = "Cumulative return")


# -->
# Black line is the median curve.
# The magenta region represents the middle 50% of curves, and the dashed red curves are the outlying functions
# Note that functional boxplots produce outlying curves rather than individual outlying returns for each day.



# ----------
# Three regions corresponding to probability levels 30, 60, and 90
par(mfrow=c(1,3))
fbplot(fit = cr, prob = 0.3, ylim = c(-20, 90), xlab = "Trading day", ylab = "Cumulative return")
fbplot(fit = cr, prob = 0.6, ylim = c(-20, 90), xlab = "Trading day", ylab = "Cumulative return")
fbplot(fit = cr, prob = 0.9, ylim = c(-20, 90), xlab = "Trading day", ylab = "Cumulative return")

