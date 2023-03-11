setwd("//media//kswada//MyFiles//R//women_queue")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Women in queues
#  - Jinkinson and Slater (1981) and Hoaglin and Tukey (1985) give the frequency distribution of the number of females
#    observed in 100 queues of length 10 in a London Underground station
# ------------------------------------------------------------------------------
data("WomenQueue", package = "vcd")

data <- WomenQueue
data



# ------------------------------------------------------------------------------
# Ord plot for Women in queues data
#
# Ord plot may be used to diagnose the form of the discrete distribution.
# Ord showed that a linear relationship:  the ratios k * n(k) / n(k-1) plotted against k should appear as a straight line,
# whose slope and intercept determin the particular distribution
#
#  slope = 0,  Intercept = + : Posisson
#  slope = -,  Intercept = + : Binomial
#  slope = +,  Intercept = + : Negative binomial
#  slope = +,  Intercept = - : Log. series
#
# black line shows the usual ordinary least squares (OLS) regression fit of frequency, n(k), on number of occurrences, k
# red line shows the weighted least sqaures fit, using weights of sqrt(n(k) - 1)
# ------------------------------------------------------------------------------
Ord_plot(data, main = "Women in queues of length 10", gp = gpar(cex = 1), pch = 16)


# -->
# The negative slope and positive intercept clearly diagnose this distribution as binomial.
# The rough estimate p = b / (1 - b) = -1.122 / (1 + 1.122) = 0.53 indicates that women are slightly more prevalent than men in these data for the London underground.
