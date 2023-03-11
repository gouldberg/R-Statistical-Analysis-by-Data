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
# fit GAM with covariate
# ------------------------------------------------------------------------------

DF$fPeriod <- factor(DF$Period)


# without an interaction term
M6 <- mgcv::gam(Dens ~ s(MeanDepth) + fPeriod, data=DF)


summary(M6)



# -->
# The results indicate that the smoother of depth is significant, and the new covariate fPeriod, is also significant.
# Estimated density value for Period 2 is 0.0021 lower than that of Period 1.



# ----------
par(mfrow = c(1,2))

plot(M5, shade = TRUE)

plot(M6, shade = TRUE)



# -->
# The smoother at MeanDepth over 4000 is differenct



# ------------------------------------------------------------------------------
# visualize the model
# ------------------------------------------------------------------------------

par(mar = c(2,2,2,2))

vis.gam(M6, theta = 120, color = "heat")



# -->
# we have two parallel lines that differ by 0.0022 (fPeriod2)
# The fact that the software displays results as a surface is somewhat misleading (we have only a line, not a surface)


# adding a loop around the vis.gam code and changing the value of theta produces an attractive video.
theta <- seq(0, 360, by = 30)

for(i in 1:length(theta)) vis.gam(M6, theta = theta[i], color = "heat")



# ------------------------------------------------------------------------------
# visualize the model by predicting values for certain depths and periods
# ------------------------------------------------------------------------------

MD1 <- data.frame(MeanDepth = seq(from = 804, to = 4865, length = 100), fPeriod = "1")

MD2 <- data.frame(MeanDepth = seq(from = 804, to = 4865, length = 100), fPeriod = "2")

P1 <- predict(M6, newdata = MD1)

P2 <- predict(M6, newdata = MD2)

MyPch <- vector(length = length(DF$Dens))
MyPch[DF$Period == 1] <- 1
MyPch[DF$Period == 2] <- 16

plot(x = DF$MeanDepth, y = DF$Dens, pch = MyPch)
lines(x = MD1$MeanDepth, y = P1, lwd = 3, col = "red")
lines(x = MD2$MeanDepth, y = P2, lwd = 3, col = "blue")



# ------------------------------------------------------------------------------
# fit GAM without an interaction term:  other implementations
# ------------------------------------------------------------------------------

# smoothers are not centered around 0, and therefore we need to drop the nominal covariate fPeriod and the intercept from the model formulation.

M8 <- gam(Dens ~ -1 + s(MeanDepth, by = as.numeric(fPeriod == "1")) + s(MeanDepth, by = as.numeric(fPeriod == "2")), data = DF)

summary(M8)



# ----------
# a different density/depth relationship in the two periods can also be modelled.
# This model contains an overall smoother f(Depth) for both periods, and f2(Depth) represents the deviation from this overall pattern
# in the second period.

M9 <- gam(Dens ~ s(MeanDepth) + s(MeanDepth, by = as.numeric(fPeriod == "2")), data = DF)

summary(M9)


par(mfrow = c(2,2), mar = c(5,6,3,2))

# estimated smoother s(MeanDepth) for the model
plot(M9, select = 1, main = "Period 1", cex.lab = 1.5)

# the deviation for Period2 from the overall pattern above
plot(M9, select = 2, main = "Period 2", cex.lab = 1.5)


# predicted values obtained by the GAM for period1
MD7.1 <- data.frame(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100), fPeriod = "1")
P9.1 <- predict(M9, newdata = MD7.1)


# predicted values obtained by the GAM for period2
MD7.2 <- data.frame(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100), fPeriod = "2")
P9.2 <- predict(M9, newdata = MD7.2)


plot(x = DF$MeanDepth[DF$Period == 1], y = DF$Dens[DF$Period == 1], ylim = c(0, 3.092395e-02),  main= "Period 1")
lines(x = MD7.1$MeanDepth, y = P9.1, lwd = 3)

plot(x = DF$MeanDepth[DF$Period == 2], y = DF$Dens[DF$Period == 2], ylim = c(0, 3.092395e-02),  main = "Period 2")
lines(x = MD7.2$MeanDepth, y = P9.2, lwd = 3)



# -->
# The residuals of all models discussed to this point show clear heterogeneity.
# The options to solve this are:
#  - (i) transforming the density data
#  - (ii) allowing for heterogeneity by using the generalized least squares equivalent in the GAM, or
#  - (iii) applying a combination of (i) and (ii)

