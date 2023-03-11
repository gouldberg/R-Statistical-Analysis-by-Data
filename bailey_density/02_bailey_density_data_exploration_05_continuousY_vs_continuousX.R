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
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(Dens ~ MeanDepth, data = DF, ylab = "Dens", cex.lab = 1.25, xlab = "MeanDepth", pch = 20, col = gray(0.7))
lines(lowess(DF$MeanDepth, DF$Dens), col = "blue", lwd = 1)


plot(Dens ~ SweptArea, data = DF, ylab = "Dens", cex.lab = 1.25, xlab = "SweptArea", pch = 20, col = gray(0.7))
lines(lowess(DF$SweptArea, DF$Dens), col = "blue", lwd = 1)


plot(TotAbund ~ MeanDepth, data = DF, ylab = "TotAbund", xlab = "MeanDepth", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(DF$MeanDepth, DF$TotAbund), col = "blue", lwd = 1)


plot(TotAbund ~ SweptArea, data = DF, ylab = "TotAbund", xlab = "SweptArea", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(DF$SweptArea, DF$TotAbund), col = "blue", lwd = 1)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications

library(ggplot2)

gg <- ggplot(DF, aes(TotAbund, MeanDepth)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "TotAbund", x = "DML")


gg



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles

par(mfrow = c(1,1))

# plot(Testisweight ~ vcdExtra::cutfac(DML, 10), data = Squid, ylab = "Testisweight", xlab = "DML", las=1)
plot(TotAbund ~ cut(MeanDepth, 10), data = DF, ylab = "TotAbund", xlab = "MeanDepth", las=1)

