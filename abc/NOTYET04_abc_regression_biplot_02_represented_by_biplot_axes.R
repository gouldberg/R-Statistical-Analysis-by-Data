setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ----------
# convert factor to numeric
ABC2 <- rapply(ABC[,c(1,4,6:11)], f = as.numeric, classes = "factor", how = "replace")

str(ABC2)



# ------------------------------------------------------------------------------
# Regression Biplots of version based on biplot axes 
# (advocated in Gower and Hand (1996) and Gower et al. (2011))
# ------------------------------------------------------------------------------

library(calibrate)


plot(ABC2$pricing, ABC2$training, pch = 20, cex = 0.8, xlab = "pricing", ylab = "training", 
     col = "darkblue", asp = 1, main = "Orthogonal Projections")
text(ABC2$pricing, ABC2$training, labels = 1:nrow(ABC2), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "darkgray")



# ----------
# calibrate to produce biplot axes and optionally show the projection lines
# Each individual's point is subject to an orthogonal projection on the VIQ axis. 
calibrate.Z <- calibrate(regfit$coef[,1], ABC2$satis, seq(-4, 4, by = 0.5), 
                         cbind(ABC2$pricing, ABC2$training))
, dp = TRUE, axiscol = "brown",
                         axislab = "satis", labpos = 3, verb = FALSE)


# ----------
# calibrated VIQ
# We can scale the fitted values back to the original scale by "destandardizing"
( VIQcal <- calibrate.Z$yt*sd(BrainIQ$VIQ) + mean(BrainIQ$VIQ) )



# ----------
# From Biplot projections
# Pick Participant #20 is quite tall and has an average brain size
# He/She should have the lowest VIQ (based on the projected VIQ)
# Participant #11 is of average height and has the biggest brain, and the plot sayes that he/she should have the highest VIQ.
BrainIQ[c(20, 11), -c(1, 4)]



# But smallest VIQ is 71  --> not corresponding to Participant #20
summary(BrainIQ[, 2])



# We see that the R^2 value for the VIQ regression is moderately low:  our projections are only as good as the model.
round(R2vec, 3)



# ----------
# We try weight axis (R^2 for weight = 0.505, moderately high)
plot(BrainIQ1$Height, BrainIQ1$MRI_Count, pch = 20, cex = 0.8, xlab = "Height", ylab = "MRI", 
     col = "darkblue", asp = 1, main = "Orthogonal Projections")
text(BrainIQ1$Height, BrainIQ1$MRI_Count, labels = 1:nrow(BrainIQ1), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "darkgray")

calibrate(regfit$coef[,3], BrainIQ1$Weight, seq(-2,2, by = 0.5), 
                         cbind(BrainIQ1$Height, BrainIQ1$MRI_Count), dp = TRUE, axiscol = "brown",
                         axislab = "Weight", labpos = 3, verb = FALSE)

# Participant #21 smallest, and #26 largest
BrainIQ[c(21, 26), ]

# But smallest Weight is 106 and largest is 192  --> not corresponding to Participant #21 and #26
summary(BrainIQ[, 4])

