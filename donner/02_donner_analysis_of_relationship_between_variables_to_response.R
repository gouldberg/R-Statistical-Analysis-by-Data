setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))



# ------------------------------------------------------------------------------
# Survived vs. family
# ------------------------------------------------------------------------------
# Main families in the Donner party were: Donner, Graves, Breen and Reed
# The families of Murphy, Foster, and Pike are grouped as "MurFosPik" those of Fosdick and Wolfinger are coded as "FosdWolf", and all others as "Other".
xtabs(~ family, data = data)



# For present purposes, we reduce these 10 family groups further, collapsing some of the small families into "Other", and reordering the levels.
fam <- data$family
levels(fam)[c(3,4,6,7,9)] <- "Others"

fam <- factor(fam, levels(fam)[c(1, 2, 4:6, 3)])
data$family <- fam

xtabs(~ family, data = data)



# ----------
# Plotting the distribution of survived by family
# Small families (Breen and Reed) were more likely to be survived, but others are less likely to be survived.
graphics.off()
spineplot(xtabs(~ family + survived, data = data))



# ------------------------------------------------------------------------------
# Generalized pairs plot
# ------------------------------------------------------------------------------
gpairs::gpairs(data[,c(4, 2, 3, 1)], diag.pars = list(fontsize = 20, hist.color = "gray"),
               mosaic.pars = list(gp = shading_Friendly), outer.rot = c(45, 45))


# -->
# boxplots and barcode plots for survived and age show that those who survived were generaly younger than those who perished



# ------------------------------------------------------------------------------
# Survived vs. age, by sex
#  - Is the relationship different for men and women ?  Necessary to allow for an interaction of age with sex, or separate fitted curves for men and women ?
#  - Is the relationship between survival and age well-represented in a linear logistic regression model ?
# ------------------------------------------------------------------------------
# conditional plot of the Donner data, showing the relationship of survival to age and sex.
# It is easy to see that survival among women was greater than for men, perhaps narrowing the gap among the older people,
# but the data gets thin towards the upper range of age.
graphics.off()
gg <- ggplot(data, aes(age, as.numeric(survived=="yes"), color = sex)) + ylab("Survived") + theme_bw() + 
  geom_point(position = position_jitter(height = 0.02, width = 0)) 
# add conditional linear logistic regressions
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = sex))



# The curves above assume a linear relationship between the log odds of survival and age.
# One simple way to check whether the relationship between survival and age is nonlinear is to allow a quadratic relationship with age.
# add conditional quadratic logistic regressions
# This plot is quite surprising. It suggests quite different regimes relating to survival for men and women.
# Among men, survival probability decreases steadily with age, at least after age 20.
# For women, those in the age range 10-35 were very likely to have lived, while those over 40 were almost all predicted to perish.
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ poly(x,2), alpha = 0.2, size = 2, aes(fill = sex))



# add loess smooth
# The curve for males suggests that survival also has a peak around the teenage years.
gg + stat_smooth(method = "loess", span=0.9, alpha = 0.2, size = 2, aes(fill = sex)) + coord_cartesian(ylim = c(-.05, 1.05))



# -->
# One lesson to be drawn from these graphs is that a linear logistic regression may tell only part of the story, and, for a binary response, it is not easy
# to discern whether the true relationship is linear.


# Note that:
# A technical problem with the use of the loess smoother for binary data is that it can produce fittd values outside the [0-1] interval.
# Kernel smoothers, such as the KernSmooth package avoid this problem, but are not available through ggplot2

