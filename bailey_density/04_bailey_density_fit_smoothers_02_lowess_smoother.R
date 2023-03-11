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
# LOESS smoothers
#   - In LOESS and its predecessor LOWESS (locally weighted scatterplot smoothing), a weighted linear regression
#     model is applied on the data in each window.
#     The regression model is a pth order polynomial, and the default option in most software is p = 2.
# ------------------------------------------------------------------------------

# defaulst span is 0.75, meaning that 75% of the data is used in each window.
# predict function is used to visualise the smoother and 95% confidence bands.

L1 <- loess(Dens ~ MeanDepth, data = DF, span = 0.75)

L1


summary(L1)



# ----------
P1 <- predict(L1, se = TRUE)




# ----------
par(mfrow=c(1,1))

plot(x = DF$MeanDepth, y = DF$Dens, pch = 16, xlab = MyXLab, ylab = MyYLab)

lines(x = DF$MeanDepth, y = P1$fit, lwd = 3)
lines(x = DF$MeanDepth, y = P1$fit + 2 * P1$se.fit, lwd = 3, lty = 2)
lines(x = DF$MeanDepth, y = P1$fit - 2 * P1$se.fit, lwd = 3, lty = 2)

abline(h = 0)



# -->
# Note that the confidence bands at greater depths contain negative values.



