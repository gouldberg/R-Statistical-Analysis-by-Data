setwd("//media//kswada//MyFiles//R//horse_kicks")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death by horse kick
# ------------------------------------------------------------------------------
data("HorseKicks", package = "vcd")

data <- HorseKicks

data
sum(data)



# ------------------------------------------------------------------------------
# Poissonness plot
#  - The Poissonness plot (Hoaglin, 1980) is a robust plot to sensitively determine how well a one-way table of frequencies follows a Poisson distribution.
#    It plots a quantity called a against k, designed so that the result will be points along a straight line when the data follow a Poisson distribution.
#    When the data deviate from a Poisson, the points will be curved.
#
#  - Poissonness plot has the follwing desirable features:
#      - Resistance:  a single discrepant value of n(k) affects only the point at value k (In the ORd plot it affects each of its neighbours)
#      - Comparison standard:  An approximate confidence interval can be found for each point, indicating its inherent variability and helping to judge
#        whether each point is discrepant.
#      - Influence:  Extensions of the method result in plots that show the effect of each point on the estimate of the main parameter of the distribution
#        (lambda in the Poisson)
#
#  - The open circles show the calculated observed values of the count Metameter.
#  - The smaller filled points show the centers of the confidence intevals, CI.center
#  - The dashed lines show the extent of the confidence intervals
#
#  - slope = log(lambda)
#  - intercept = - lambda
#  - If the points in this plot are close enough to a straight line, then an estimate of lambda may be obtained from the slope of the line,
#    and lambda = exp(slope) should be reasonably close in value to the MLE of lambda
# ------------------------------------------------------------------------------

dp <- distplot(data, type = "poisson", xlab = "Number of deaths", main = "Poisonness plot: Horsekicks data")

print(dp, digits = 4)



# -->
# The fitted least squares line has a slope of -0.431, which would indicate lambda = exp(-0.431) = 0.65.
# This compares well with the MLE, lambda = 0.61



# ------------------------------------------------------------------------------
# leveled version plot, specifying lambda
#
#  - Reference line is horizontal, making comparison of the points with the line easier.
# ------------------------------------------------------------------------------
distplot(data, type = "poisson", lambda = 0.61, xlab = "Number of deaths", main = "Leveled Poisonness plot")
