setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice")
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

# The data were sampled at 14 tstations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# fitting smoothers for a quadratic spline regression model using different number of knots and compare by AIC
# ------------------------------------------------------------------------------
probs1 <- seq(0, 1, length = 3);  QD1 <- quantile(BL$Depth, probs1)
probs3 <- seq(0, 1, length = 5);  QD3 <- quantile(BL$Depth, probs3)
probs5 <- seq(0, 1, length = 5);  QD5 <- quantile(BL$Depth, probs3)
probs7 <- seq(0, 1, length = 7);  QD7 <- quantile(BL$Depth, probs7)
probs9 <- seq(0, 1, length = 9);  QD9 <- quantile(BL$Depth, probs9)
probs11 <- seq(0, 1, length = 11);  QD11 <- quantile(BL$Depth, probs11)
probs13 <- seq(0, 1, length = 12);  QD13 <- quantile(BL$Depth, probs13)
probs15 <- seq(0, 1, length = 15);  QD15 <- quantile(BL$Depth, probs15)
probs17 <- seq(0, 1, length = 17);  QD17 <- quantile(BL$Depth, probs17)
probs19 <- seq(0, 1, length = 19);  QD19 <- quantile(BL$Depth, probs19)
probs21 <- seq(0, 1, length = 21);  QD21 <- quantile(BL$Depth, probs21)
probs31 <- seq(0, 1, length = 31);  QD31 <- quantile(BL$Depth, probs31)


rhs2 <- function(x, TH) ifelse(x >= TH, (x-TH)^2,0)


M5.1 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD1[2]), data = BL)
M5.3 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD3[2]) + rhs2(Depth, QD3[3]) + rhs2(Depth, QD3[4]), data = BL)
M5.5 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD5[2]) + rhs2(Depth, QD5[3]) + rhs2(Depth, QD5[4]) + rhs2(Depth, QD5[5]), data = BL)
M5.7 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD7[2]) + rhs2(Depth, QD7[3]) + rhs2(Depth, QD7[4]) + rhs2(Depth, QD7[5]) + rhs2(Depth, QD7[6]), data = BL)
M5.9 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD9[2]) + rhs2(Depth, QD9[3]) + rhs2(Depth, QD9[4]) + rhs2(Depth, QD9[5]) + rhs2(Depth, QD9[6]) + rhs2(Depth, QD9[7]), data = BL)
M5.11 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD11[2]) + rhs2(Depth, QD11[3]) + rhs2(Depth, QD11[4]) + rhs2(Depth, QD11[5]) + rhs2(Depth, QD11[6]) + rhs2(Depth, QD11[7]) + rhs2(Depth, QD11[8]), data = BL)
M5.13 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD13[2]) + rhs2(Depth, QD13[3]) + rhs2(Depth, QD13[4]) + rhs2(Depth, QD13[5]) + rhs2(Depth, QD13[6]) + rhs2(Depth, QD13[7]) + rhs2(Depth, QD13[8]) + rhs2(Depth, QD13[9]), data = BL)
M5.15 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD15[2]) + rhs2(Depth, QD15[3]) + rhs2(Depth, QD15[4]) + rhs2(Depth, QD15[5]) + rhs2(Depth, QD15[6]) + rhs2(Depth, QD15[7]) + rhs2(Depth, QD15[8]) + rhs2(Depth, QD15[9]) + rhs2(Depth, QD15[10]), data = BL)
M5.17 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD17[2]) + rhs2(Depth, QD17[3]) + rhs2(Depth, QD17[4]) + rhs2(Depth, QD17[5]) + rhs2(Depth, QD17[6]) + rhs2(Depth, QD17[7]) + rhs2(Depth, QD17[8]) + rhs2(Depth, QD17[9]) + rhs2(Depth, QD17[10]) + rhs2(Depth, QD17[11]), data = BL)
M5.19 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD19[2]) + rhs2(Depth, QD19[3]) + rhs2(Depth, QD19[4]) + rhs2(Depth, QD19[5]) + rhs2(Depth, QD19[6]) + rhs2(Depth, QD19[7]) + rhs2(Depth, QD19[8]) + rhs2(Depth, QD19[9]) + rhs2(Depth, QD19[10]) + rhs2(Depth, QD19[11]) + rhs2(Depth, QD19[12]), data = BL)
M5.21 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD21[2]) + rhs2(Depth, QD21[3]) + rhs2(Depth, QD21[4]) + rhs2(Depth, QD21[5]) + rhs2(Depth, QD21[6]) + rhs2(Depth, QD21[7]) + rhs2(Depth, QD21[8]) + rhs2(Depth, QD21[9]) + rhs2(Depth, QD21[10]) + rhs2(Depth, QD21[11]) + rhs2(Depth, QD21[12]) + rhs2(Depth, QD21[13]), data = BL)
M5.31 <- lm(Sources ~ Depth  + I(Depth^2) + rhs2(Depth, QD31[2]) + rhs2(Depth, QD31[3]) + rhs2(Depth, QD31[4]) + rhs2(Depth, QD31[5]) + rhs2(Depth, QD31[6]) + rhs2(Depth, QD31[7]) + rhs2(Depth, QD31[8]) + rhs2(Depth, QD31[9]) + rhs2(Depth, QD31[10]) + rhs2(Depth, QD31[11]) + rhs2(Depth, QD31[12]) + rhs2(Depth, QD31[13]) + rhs2(Depth, QD31[14]), data = BL)

P5.1 <- predict(M5.1, newdata = MD)
P5.3 <- predict(M5.3, newdata = MD)
P5.5 <- predict(M5.5, newdata = MD)
P5.7 <- predict(M5.7, newdata = MD)
P5.9 <- predict(M5.9, newdata = MD)
P5.11 <- predict(M5.11, newdata = MD)
P5.13 <- predict(M5.13, newdata = MD)
P5.15 <- predict(M5.15, newdata = MD)
P5.17 <- predict(M5.17, newdata = MD)
P5.19 <- predict(M5.19, newdata = MD)
P5.31 <- predict(M5.31, newdata = MD)

AllP <- c(P5.1, P5.3, P5.5, P5.7, P5.9, P5.11, P5.13, P5.15, P5.17, P5.19, P5.31)
AllD <- rep(MD$Depth, 11)
AllID <- rep(c("1 inner knot", "3 inner knot", "5 inner knot", "7 inner knot", "9 inner knot", "11 inner knot", "13 inner knot", "15 inner knot", "17 inner knot", "19 inner knot", "21 inner knot", "31 inner knot"), each = length(P5.3))
AllID2 <- factor(AllID, levels = c("1 inner knot", "3 inner knot", "5 inner knot", "7 inner knot", "9 inner knot", "11 inner knot", "13 inner knot", "15 inner knot", "17 inner knot", "19 inner knot", "21 inner knot", "31 inner knot"))

xyplot(AllP ~ AllD | AllID2, col = 1, type = "l",  lwd = 3,
       xlab = list(label = "Depth", cex = 1.5),
       ylab = list(label = "Sources", cex = 1.5),
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2))
)



# ----------
# compare models
AIC(M5.3, M5.7, M5.9, M5.11, M5.13, M5.15, M5.17, M5.19, M5.21, M5.31)



# -->
# The models with 3, 5, and 7 knots all have the lowest AIC.
# The differences between curves are surprisingly small.

