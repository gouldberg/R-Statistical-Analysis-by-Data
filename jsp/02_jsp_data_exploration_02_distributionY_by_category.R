setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)




# ------------------------------------------------------------------------------
# data exploration: math, raven by school, social  by boxplot
# ------------------------------------------------------------------------------

boxplot(raven ~ school, data = jspr, varwidth = TRUE, ylab = "raven", xlab = "school", cex.lab = 1.25)

boxplot(math ~ school, data = jspr, varwidth = TRUE, ylab = "raven", xlab = "school", cex.lab = 1.25)

boxplot(english ~ school, data = jspr, varwidth = TRUE, ylab = "raven", xlab = "school", cex.lab = 1.25)




# ----------
boxplot(math ~ social, data = jspr, varwidth = TRUE, ylab = "raven", xlab = "social", cex.lab = 1.25)

boxplot(english ~ social, data = jspr, varwidth = TRUE, ylab = "raven", xlab = "social", cex.lab = 1.25)




# ------------------------------------------------------------------------------
# data exploration: math, raven by school, social  by dotplot
# ------------------------------------------------------------------------------
# reorder() the school in ascending order of the variable of interest in terms of the mean


library(lattice)

dotplot(reorder(school, raven) ~ raven, data = jspr, jitter.y = TRUE, ylab = "school", main = "Dotplot of \'raven\' for School")

dotplot(reorder(school, math) ~ math, data = jspr, jitter.y = TRUE, ylab = "school", main = "Dotplot of \'math\' for School")

dotplot(reorder(school, english) ~ english, data = jspr, jitter.y = TRUE, ylab = "school", main = "Dotplot of \'english\' for School")


# -->
# variance of math is larger than variance of raven



# ----------
dotplot(reorder(social, math) ~ math, data = jspr, jitter.y = TRUE, ylab = "social", main = "Dotplot of \'math\' for School")

dotplot(reorder(social, english) ~ english, data = jspr, jitter.y = TRUE, ylab = "social", main = "Dotplot of \'english\' for School")



# ----------
# calculate group mean and variance
by(jspr$math, jspr$social, FUN = mean)
# aggregate(jspr$math, by = list(jspr$social), FUN = "mean")

aggregate(jspr$math, by = list(jspr$social), FUN = "var")



# -->
# Note that the score of math is ordered almost by social class and the variance of math is small by small number of social class !!!



# ------------------------------------------------------------------------------
# data exploration:  raven, math by school, social  by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)


ggplot(jspr, aes(x = school, y = raven)) + geom_boxplot()

ggplot(jspr, aes(x = school, y = math)) + geom_boxplot()

ggplot(jspr, aes(x = social, y = math)) + geom_boxplot()

ggplot(jspr, aes(x = social, y = raven)) + geom_boxplot()
