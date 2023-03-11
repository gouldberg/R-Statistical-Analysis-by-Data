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
# data exploration:  log Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(d, aes(rugged, rgdppc_2000)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "rgdppc_2000", x = "rugged") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20))


gg + facet_wrap(~ cont_africa)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = log(rgdppc_2000) ~ rugged | cont_africa

coplot(formula, data = d, ylab = "rgdppc_2000", xlab = "rugged", las=1)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

formula = log(rgdppc_2000) ~ rugged | cont_africa

xyplot(formula, data = d)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula = log(rgdppc_2000) ~ rugged | cont_africa

car::scatterplot(formula, data = d)



