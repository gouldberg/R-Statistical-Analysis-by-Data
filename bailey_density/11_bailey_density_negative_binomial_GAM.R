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
# Negative Binomial GAM with offset
# ------------------------------------------------------------------------------

# The gam modelling function is designed to be able to use the negbin family or the nb function designed for integrated estimation of parameter theta (in this case, k)
# With negbin then if 'performance iteration' is used for smoothing parameter estimation, then smoothing parameters are chosen by GCV and 
# theta is chosen in order to ensure that the Pearson estimate of the scale parameter is as close as possible to 1, the value that the scale parameter should have.

# If 'outer iteration' is used for smoothing parameter selection with the nb family then theta is estimated alongside the smoothing parameters by ML or REML. 

library(mgcv)

mod.nbgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = nb(), data = DF)

# mod.nbgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = negbin(theta = c(1,10)), data = DF)


summary(mod.nbgam)


# -->
# effective degree of freedoms are 2.81 and 3.019 for each smoothers



mod.nbgam$family$getTheta(TRUE)



# -->
# The estimated value of k (theta) is 2.048
# The period effect is significant, and both smoothers are significant.
# Dispersion parameter is almost 1.



# ----------
# For comparison, single smoother

mod.nbgam2 <- gam(TotAbund ~ s(MeanDepth) + fPeriod + offset(LogSA), family = nb(theta = 2.048), data = DF)


AIC(mod.nbgam, mod.nbgam2)



# -->
# AIC values indicate that the model with two smoothers is superior (but small difference) to the model with one smoother.



# ------------------------------------------------------------------------------
# plot smoothers
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(1,2), mar = c(5,6,3,2))

plot(mod.nbgam, select = 1, main = "Period 1", shade = TRUE)

plot(mod.nbgam, select = 2, main = "Period 2", shade = TRUE)



# -->
# For Period 1 we have a pattern that is slightly non-linear, as is also reflected in the 2.8 degrees of freedom.
# Second smoother is less linear, and has higher degrees of freedom.
# The smoothers are presented on the scale of the link function. This means that a straight line may represent an exponential relationship
# on the scale of the raw data.


