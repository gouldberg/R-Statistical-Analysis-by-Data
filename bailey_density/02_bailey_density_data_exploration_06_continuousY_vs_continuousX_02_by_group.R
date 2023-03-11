# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot by group
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications

library(ggplot2)

gg <- ggplot(DF, aes(TotAbund, MeanDepth)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "TotAbund", x = "DML")


gg + facet_wrap(~ Year > 1990)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

formula = TotAbund ~ MeanDepth | Year > 1990

coplot(formula, data = DF, ylab = "TotAbund", xlab = "MeanDepth", las=1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# relationship between fish density and mean sampling depth
# ------------------------------------------------------------------------------

MyXLab <- "Mean sampling depth (m)"

MyYLab <- expression(paste("Fish density (",m^{-2}, ")"))

par(mfrow=c(1,1), mar = c(5,5,2,3))

plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab, col= DF$Period, pch = ifelse(DF$Year > 1990, 20, 1), cex.lab = 1.5)



# -->
# scatterplot of the data clearly shows a non-linear pattern.
# Note that the variation decreases at greater depth.

