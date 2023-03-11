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
# Data Exploration:  Conditional Plot by ggplot with confidence intervals, but here with conditioned by other variable
# binary Y vs. continuous X
# ------------------------------------------------------------------------------


par(mfrow=c(1,1))


gg <- ggplot(ICU, aes(x = age, y = (died == "Yes")*1), color = admit) + xlim(5, 95) + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0.2)) +
  stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha = 0.2, size = 2, fullrange = TRUE, aes(fill = admit))

gg


# ggplots can add other stratifying variables by facet_grid()
gg + facet_wrap(~ uncons)

