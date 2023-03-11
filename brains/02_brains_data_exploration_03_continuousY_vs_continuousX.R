setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ----------
# Since the distribution of both brain size and body weight are highly skewed, a log transformation was applied to both
# variables to give transformed variables

brains <- transform(brains, lbrain = log(brain), lbody= log(body))




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

with(brains, plot(lbrain ~ lbody, ylab = "log brain", xlab = "log body"))
lines(lowess(brains$lbody, brains$lbrain), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale

library(ggplot2)

gg <- ggplot(brains, aes(lbody, lbrain)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "exra", x = "nao")


gg



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles
par(mfrow = c(1,1))

plot(lbrain ~ cut(lbody, 10), data = brains, ylab = "lbrain", xlab = "lbody", las=1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X   by scatterplot
# ------------------------------------------------------------------------------

formula <- lbrain ~ lbody

car::scatterplot(formula, data = brains)


# -->
# A normal error linear regression model of lbrain against lbody has a highly significant slope for lbody but
# it is believed that the data may represent different stages of evolution and so a mixture model is fitted to the data.
