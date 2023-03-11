setwd("//media//kswada//MyFiles//R//catt")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CATT
# ------------------------------------------------------------------------------

# VAS Longitudinal Measurement
Data <- read.csv("va.csv")


# VAS Baseline Measurements
Base <- read.csv("bv.csv")


# Other Subject Information
general_info <- read.csv("gi.csv")


str(Data)

str(Base)

str(general_info)



# ----------
car::some(Data)

car::some(Base)

car::some(general_info)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(mgcv)


# number of lambdas
r = 20
( lambda_all <- 10 ^ seq(-8, 2, length = r) )



# ----------
# Mean Prediction Error
MPE <- numeric(0)


# check fir for each lambda
for(lambda in lambda_all){
  
  x <- Fit[,1];  y <- Fit[,2]
  
  GAMF <- gam(y ~ s(x, sp = lambda))
  
  pred <- predict(GAMF, newdata = data.frame(x = Test[,1]))
  
  MPE <- c(MPE, mean((pred - Test[,2]) ^ 2))
}


par(mfrow = c(1,1))
plot(MPE ~ lambda_all, type = "b")



# ----------
# select best lambda and refit with all data
( lambda <- lambda_all[which.min(MPE)] )

All <- rbind(Fit, Test)

x <- All[,1];  y <- All[,2]

GAMF <- gam(y ~ s(x, sp = lambda))



# ----------
plot(GAMF)



# ----------
# smoothing parameter is more increased
GAMF2 <- gam(y ~ s(x, sp = lambda * 10^9))

plot(GAMF2)



# -->
# A significant increase in VAS as the study progresses, but the increase levels off.
# This is likely due to the fact that all subjects were assigned to some treatment, i.e. there was no control group.

# Furthermore, the subjects had not previously received any treatment for their AMD.
# Thus it is reasonable to expect an increase in visual acuity as the study progresses. with a leveling-off period after the treatments have reached
# their potential.

# One potential point of concern is the baseline measurement.
# This measurement is made before applying any treatment, and as we move to the first observation which includes a treatment,
# we see a sharp jump/
# Subsequent increases seem to be substantially smaller, and there is a clear leveling off. Thus, there is a very real concern that
# there is a discontinuity at the beginning, or at the very least, a very rapid increase.

# Such artifacts can make choosing tuning parameters difficult, as different parameter values might produce estimates which fit well in certain regions / domains,
# but not others.

# We need to explore a potential fix to this problem by omitting the baseline measurement,
# and instead subtracting it from each subsequent observation.
