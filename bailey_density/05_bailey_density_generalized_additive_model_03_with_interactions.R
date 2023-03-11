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
# fit GAM with interaction term
# ------------------------------------------------------------------------------

M7 <- mgcv::gam(Dens ~ s(MeanDepth, by = fPeriod) + fPeriod, data = DF)

summary(M7)



# -->
# We have two intercepts and two smoothers.
# The smoother f1(Depth) for Period 1 has 4.2 degrees of freedom
# and the smoother f2(Depth) has 1.1 degrees of freedom, which is essentially a straight line.



# ------------------------------------------------------------------------------
# visualize the model
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), mar = c(2,2,2,2))

vis.gam(M7, theta = 120, color = "heat")



# ------------------------------------------------------------------------------
# visualize the model by predicting values for certain depths and periods
# ------------------------------------------------------------------------------

MD7.1 <- data.frame(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100), fPeriod = "1")

MD7.2 <- data.frame(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100), fPeriod = "2")

P7.1 <- predict(M7, newdata=MD7.1)

P7.2 <- predict(M7, newdata=MD7.2)



par(mfrow = c(2, 2))

plot(M7, select = 1, main = "Period 1")

plot(M7, select = 2, main = "Period 2")


plot(x = DF$MeanDepth[DF$Period == 1], y = DF$Dens[DF$Period == 1], ylim = c(0, 0.03),  main = "Period 1")
lines(x = MD7.1$MeanDepth, y = P7.1, lwd=3)

plot(x = DF$MeanDepth[DF$Period == 2], y = DF$Dens[DF$Period == 2], ylim = c(0, 0.03),  main = "Period 2")
lines(x = MD7.2$MeanDepth, y = P7.2, lwd = 3)



# -->
# Note that the smoothers are centred around zero.
# The difference between observed and fitted values is large for one observation in Period 2.
# We still have heterogeneity and should abondon the model.



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

# compare the model with and without interactions by AIC
# Those models are not nested, so we can not use anova()

AIC(M6, M7)


# -->
# The models have similar AIC values, in which case the model with one smoother is preferred over the model with two smoothers (one per period)
# as it is easier to interpret.

