setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by ggplot with confidence intervals
# binary Y vs. continuous X
# ------------------------------------------------------------------------------


par(mfrow=c(1,1))


# Plotting logistic regression by ggplots --> This method does logistic regression and visualization simultaneously !!!
# compared to linear regression and lowess curve

gg <- ggplot(ICU, aes(x = age, y = (died == "Yes")*1)) + xlim(5, 95) + geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha = 0.1, fill = "blue", size = 2, fullrange = TRUE)

gg <- gg + stat_smooth(method = "lm", se = FALSE, size = 1.2, color = "black", fullrange = TRUE)

gg <- gg + stat_smooth(method = "lowess", se = FALSE, span = 0.95, colour = "red", size = 1.2)

gg

