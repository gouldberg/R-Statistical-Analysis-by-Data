setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------

# Here we need na.omit()

plot(rgdppc_2000 ~ rugged, log = "y",
     data = na.omit(d), ylab = "rgdppc_2000", xlab = "rugged", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(na.omit(d)$rugged, na.omit(d)$rgdppc_2000), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

xyplot(rgdppc_2000 ~ rugged, data = d)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(d, aes(x = rugged, y = rgdppc_2000)) + xlab("rugged") + ylab("rgdppc_2000") + geom_point(position = position_jitter(), alpha = 0.3) + stat_smooth() +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20))
  


# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by scatterplot
# ------------------------------------------------------------------------------

formula <- log(rgdppc_2000) ~ rugged

car::scatterplot(formula, data = d)


