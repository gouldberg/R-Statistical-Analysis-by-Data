setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Heger Priede
# ------------------------------------------------------------------------------
BL <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HegerPriede.txt", header = TRUE)


str(BL)

names(BL)



# ------------------------------------------------------------------------------
# relationship between Sources and Depth
# ------------------------------------------------------------------------------

# The data were sampled at 14 stations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# Additive models with multiple smoothers
# ------------------------------------------------------------------------------
# a covariate that we have not mentioned is fluorescence, which is chlorophyll (micrograms per litre). It is denoted by flcugl.
# Note that there is one observation with a comparatively larger flcugl value
par(mfrow = c(1,2), mar = c(5,5,3,3))
plot(x = BL$Depth, y = BL$flcugl, xlab = "Depth", ylab = "flcugl", cex.lab = 1.5)
plot(x = BL$flcugl, y = BL$Sources, xlab = "flcugl", ylab = "Sources", cex.lab = 1.5)



# ----------
# we remove the observation
BL2 <- BL[BL$flcugl < 0.03, ]



# ----------
# cubic smoothing splines
# To deal with the fact that both smoothers contain an intercept, and there is also the intercept alpha, we drop the intercept from
# each smoother and centre them around zero.
# This is the identifiability issue.
G5 <- gam(Sources ~ s(Depth, bs = "cr") + s(flcugl, bs = "cr") + factor(Station), data = BL2)


par(mfrow = c(1,2))
plot(G5)


# -->
# fclugl smoother shows a gradual increase, followed by a sudden decrease at 3/4 of the grandient.
# 


summary(G5)


# -->
# Both smoothers are significant at the 5% level, but we are ignoring the eddy effect.
# Perhaps the fclugl effect also changes if there is an eddy effect.



# ----------
G6 <- gam(Sources ~ s(Depth, bs = "cr"), data = BL2)
G7 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy), bs = "cr") + s(flcugl, bs = "cr"),  data = BL2)
G8 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + s(flcugl, by = factor(Eddy), bs = "cr"),  data = BL2)
G9 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + flcugl, data = BL2)
G10 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + flcugl * factor(Eddy), data = BL2)
G11 <- gam(Sources ~ factor(Station) + te(Depth, flcugl), data = BL2)
G12 <- gam(Sources ~ factor(Station) + te(Depth, flcugl, by = Eddy), data = BL2)

AIC(G5, G6, G7, G8, G9, G10, G11, G12)



# ----------
par(mfrow = c(2,2))
plot(G8, scale = FALSE)


# -->
# We find the smoothers for flcugl suspiciou; note the wider confidence bands for the second flcugl smoother.
# This is because the minimum flcugl values differ fro stations where there is an eddy.
# The best solution is to truncate the raw data and re-apply the model.


