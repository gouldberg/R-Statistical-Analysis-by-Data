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
# Model 4:  Include "Area"
# ------------------------------------------------------------------------------

M4 <- mgcv::gam(Worms ~ s(Length, by = Sex) + Sex + Area, data = Par, family = binomial)


AIC(M1, M2, M3, M4)


# -->
# Model 4 is considerably bettern than other models 



# ----------
summary(M4)



# -->
# Explained deviance is 29.6%
# Effective degrees of freedom is 1 for females and males ...
# The estimated area value for the San Matias level is -3.42.
# Relatively large negative intercept indicates that the fitted values for the San Matias samples are close to zero.



# ----------
# Estimated smoothers
par(mfrow = c(1,2), mar = c(5,5,3,3))

plot(M4, resid = TRUE,  shade = TRUE, cex = 0.5,  cex.lab = 1.5,  pch = 16)



# -->
# Curiously, both smoothers are linear .....
# All lines intersect ata single point in the centre of the length gradient, due to a constraint making smoothers unique (centred around 0)
# inside the GAM algorithm.
# The graph implies the slope of the lines differ.



# ----------
Raise <- (1 - Par$Worms) * as.numeric(Par$Area)/50

Lower <- Par$Worms * as.numeric(Par$Area)/50

MyPch <- rep(1, nrow(Par))

MyPch[Par$Area == "SanMatias"] <- 16

par(mfrow = c(1,1), mar = c(5,5,3,3))
plot(x = Par$Length, 
     y = Par$Worms + Raise - Lower,
     xlab = "Length",
     ylab = ylab.name,
     col = 1, 
     pch = MyPch,
     cex.lab = 1.5)

polygon(c(MD$Length, rev(MD$Length)),
        c(SE.Low, rev(SE.UP)),
        col = grey(0.5), border=NULL,
        density = 50   )

lines(x = MD$Length, y = Fit, lwd = 3)



# -->
# Filled circles are observations from San Matias, and the open circles are from the other two zones.
# Most San Matias samples are equal to zero.
# The model 1 captures this pattern with the length smoother.
# But apparently if we include area as a covariate in M4, the San Matias level is used to provide a much better fit for these zeros,
# and the length smoother in M4 becomes linear.
