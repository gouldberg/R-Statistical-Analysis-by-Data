setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(d, aes(age, height)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "height", x = "age")


gg + facet_wrap(~ Subject)




# ----------
d %>% ggplot(aes(age, height, group=Subject, color=as.factor(Subject))) + 
  geom_line() + 
  geom_point(size=2) + 
  ggtitle("Boy's height as function of age", subtitle = "Each line corresponds to a particuar boy(subject). Age is centered and standardized.")



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = height ~ age | Subject

coplot(formula, data = d, ylab = "height", xlab = "age", las=1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

formula = height ~ age | Subject

xyplot(formula, data = d, type = c("p", "g", "smooth"))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula <- height ~ age | Subject

car::scatterplot(formula, data = d)



