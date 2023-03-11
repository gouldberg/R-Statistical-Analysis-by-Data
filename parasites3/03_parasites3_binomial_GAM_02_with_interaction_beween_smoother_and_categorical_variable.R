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
# Fit binomial GAM adding other covariate
# ------------------------------------------------------------------------------

M2 <- mgcv::gam(Worms ~ s(Length) + Sex,  data = Par, family = binomial)


anova(M2)


AIC(M1, M2)


# -->
# M1 is Slightly bettern than M2



# ------------------------------------------------------------------------------
# Fit binomial GAM with interaction between smoother and a categorical variable
# ------------------------------------------------------------------------------

M3 <- mgcv::gam(Worms ~ s(Length, by = Sex) + Sex,  data = Par, family = binomial)         


summary(M3)


AIC(M1, M2, M3)



# -->
# The smoother for the females has 8.2 degrees of freedom and that for the males as 2.75, indicating a more linear length effect for the males than females/
# Model M3 is a slight imrprovement over model M1.



# ----------
# Estimated smoothers
par(mfrow = c(1,2), mar = c(5,5,3,3))

plot(M3, resid = TRUE,  shade = TRUE, cex = 0.5,  cex.lab = 1.5,  pch = 16)



# -->
# The length smoother for the females is similar to that in M1, but the smoother for the males appears to differ.


