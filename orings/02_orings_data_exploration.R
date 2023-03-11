setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

# the proportion of damaed O-rings against temperature
par(mfrow=c(1,1))

plot(damage/6 ~ temp, orings, xlim = c(25, 85), ylim = c(0,1), xlab = "Temperature", ylab = "Prob of damage")




# ------------------------------------------------------------------------------
# data exploration by ggplot2  (logistic regression with 95% confidence interval)
# ------------------------------------------------------------------------------

library(ggplot2)


ggplot(orings, aes(x = temp, y = damage / 6)) + xlim(30, 81) + xlab("Temperature (F)") + ylab("O-Ring Failure Probability") +
  geom_point(position = position_jitter(width = 0, height = 0.01), aes(size = 2)) +
  theme(legend.position = "none") +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), se = TRUE, fill = "blue", aes(weight = 6), fullrange = TRUE, alpha = 0.2, size = 2)
