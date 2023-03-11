setwd("//media//kswada//MyFiles//R//parasites3")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  parasites3
# ------------------------------------------------------------------------------

Par <- read.table(file = "Parasites3.txt", header = TRUE)


str(Par)

dim(Par)



# ----------
Par$Worms <- Par$Elytrophalloides_oatesi



# ------------------------------------------------------------------------------
# Fit binomial GAM
# ------------------------------------------------------------------------------

M1 <- mgcv::gam(Worms ~ s(Length), data = Par, family = binomial)


summary(M1)



# -->
# The smoother has 6.8 degrees of freedom and is significantly different from zero at the 5% level.
# Keep in mind that this p-value is approximate.
# Deviance explained is 10.6%, which is low.



deviance(M1)

logLik(M1)

AIC(M1)



# ------------------------------------------------------------------------------
# Model validation:  Pearson residuals vs. fitted values
# ------------------------------------------------------------------------------

# Pearson residuals vs. fitted values
E1 <- resid(M1, type = "pearson")

F1 <- fitted(M1, type = "response")

par(mfrow = c(1,1), mar = c(5,5,3,3))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals",  pch = 16)
abline(h = 0)




# ------------------------------------------------------------------------------
# Model validation:  Smoothers
# ------------------------------------------------------------------------------

# shade = TRUE:  95% point-wise confidence bands
plot(M1, resid = TRUE, shade = TRUE, cex = 0.5, pch = 16)



# -->
# The shape of the smoother shows an increase at length 32 cm, constant values between 38 cm and 47 cm,
# and wider confidence bands for high lenght values.

# The problem is that the smoother is not on the scale of the raw data.



# ------------------------------------------------------------------------------
# Model validation:  Probability of presence vs. covariate
# ------------------------------------------------------------------------------

range(Par$Length)

ylab.name = expression(paste("Probability of presence of ", italic("E. oatesi"),  sep=""))

MD <- data.frame(Length = seq(29.5, 57, length = 100))

P1 <- predict(M1, newdata = MD, se = TRUE, type = "link")

SE.UP  <- exp(P1$fit + 2 * P1$se.fit) / (1+exp(P1$fit + 2 * P1$se.fit))

SE.Low <- exp(P1$fit - 2 * P1$se.fit) / (1+exp(P1$fit - 2 * P1$se.fit))

Fit    <- exp(P1$fit) / (1+exp(P1$fit))



# ----------
plot(x = Par$Length,  
     y = Par$Worms,
     xlab = "Length",
     ylab = ylab.name,
     pch = 16, col = grey(0.3))

polygon(c(MD$Length, rev(MD$Length)),
        c(SE.Low, rev(SE.UP)),
        col = grey(0.5),border=NULL,
        density = 50   )

lines(MD$Length, Fit, lwd = 3)
