setwd("//media//kswada//MyFiles//R//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------
Owls <- read.table(file = "//media//kswada//MyFiles//references//ZuurDataMixedModelling//Owls.txt", header = TRUE)

str(Owls)


Owls$NCalls <- Owls$SiblingNegotiation

Owls$LBroodSize <- log(Owls$BroodSize)

Owls$fNest<-factor(Owls$Nest)



# ------------------------------------------------------------------------------
# Poisson Generalized Additive Mixed Model
#   - Try to add a random slope for arrival time or even a generalized additive mixed model in which arrival time is fitted as a smoother.
# ------------------------------------------------------------------------------
library(mgcv)

O4.gamm <- gamm(NCalls ~ offset(LBroodSize) + FoodTreatment + s(ArrivalTime), random = list(fNest = ~1), data = Owls, family = poisson)

summary(O4.gamm)


# -->
# The object O4.gamm has two items, a $gam and a $lme bit.


summary(O4.gamm$gam)
# summary(O4.gamm$gam, cor = FALSE)

anova(O4.gamm$gam)

plot(O4.gamm$gam)



# -->
# Note that the shape of the smoother is very similar to the one before.


# ----------
summary(O4.gamm$lme)


# -->
# It is a little intimidating though !
# This reason for this is that gamm is repeatedly calling glmmPQL if a non-Gaussian distribution, or non-identity link function, is used.




O4.gamm <- gamm(NCalls ~ offset(LBroodSize) + FoodTreatment + s(ArrivalTime), random = list(fNest = ~ 1), data = Owls, family = quasi(link = log, variance = "mu"))

summary(O4.gamm$gam)



# ----------
# Randome effects part gives information on variance 1 / lambda
intervals(O4.gamm$lme, which="var-cov")



# ----------
# Pearson residuals versus square-root transformed fitted values
eta <- fitted(O4.gamm$lme) + Owls$LBroodSize
fv  <- exp(eta)
res.raw <- Owls$NCalls - fv
res.P <- (Owls$NCalls - fv) / sqrt(fv)
sd(res.P)

plot(sqrt(fv), res.P)



# ----------
# Standardized residuals versus square-root transformed fitted values
E4 <- resid(O4.gamm$lme, type = "normalized")
plot(sqrt(fv), E4)

