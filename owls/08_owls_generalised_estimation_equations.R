setwd("//media//kswada//MyFiles//R//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------
Owls <- read.table(file = "//media//kswada//MyFiles//references//ZuurDataMixedModelling//Owls.txt", header = TRUE)

str(Owls)

Owls$LogNeg <- log10(Owls$NegPerChick + 1)

Owls$NCalls <- Owls$SiblingNegotiation

Owls$LBroodSize <- log(Owls$BroodSize)



# ------------------------------------------------------------------------------
# GEE by compound correlation structure between all observations from the same nest
# ------------------------------------------------------------------------------
library(geepack)


# Use the compound correlation structure, which is called 'exchangeable' within the context of the GEE.
# This assumes that all observations from the nest are correlation with the value of alpha
Form <- formula(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent * ArrivalTime)

O2 <- geeglm(Form, data = Owls, family = poisson, id = Nest, corstr = "exchangeable")

summary(O2)


# -->
# estimated value of alpha is 0.058, which is rather small.



# ------------------------------------------------------------------------------
# GEE:  AR1 correlation structure between sequntial observations from same nest per night.
#   - We assume that from the owls point of view, time may be regularly spaced and use NestNight
# ------------------------------------------------------------------------------

# The problem is that the data file does not contain a column that identifies the group of observations from the same night and nest,
# hence we have to make it:  "NestNight"
Owls$NestNight <- factor(ifelse(Owls$FoodTreatment == "Deprived",
                              paste(Owls$Nest, ".Dep",sep=""),
                              paste(Owls$Nest, ".Sat",sep="")))


head(Owls)



# ----------
O3 <- geeglm(Form, data = Owls, family = poisson, id = NestNight, corstr = "ar1")

summary(O3)



# ----------
# To figure out whether we need the two 2-way interactions, we can drop each of them in turn, apply the Wald test, and remove the least
# significant variable.
O3.A <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + FoodTreatment + SexParent * ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O3.B <- geeglm(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")


anova(O3, O3.A)
anova(O3, O3.B)

summary(O3.A)
summary(O3.B)


# -->
# The sex of the parent and food treatment interaction is the least significant term and drop



# ----------
O4 <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + FoodTreatment + SexParent * ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O4.A <-geeglm(NCalls ~ offset(LBroodSize) + SexParent + FoodTreatment + SexParent + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O4.B <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + SexParent * ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")


anova(O4, O4.A)
anova(O4, O4.B)

summary(O4.A)
summary(O4.B)


# -->
# The sex of the parent and arrival time interaction is not significant and drop



# ----------
O5 <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + FoodTreatment + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O5.A <- geeglm(NCalls ~ offset(LBroodSize) + FoodTreatment + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O5.B <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O5.C <- geeglm(NCalls ~ offset(LBroodSize) + SexParent + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")

anova(O5, O5.A)
anova(O5, O5.B)
anova(O5, O5.C)


summary(O5.A)
summary(O5.B)
summary(O5.C)



# ----------
# Final model
O6 <- geeglm(NCalls ~ offset(LBroodSize) + FoodTreatment + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O6.A <- geeglm(NCalls ~ offset(LBroodSize) + ArrivalTime, data = Owls, family = poisson, id = NestNight, corstr = "ar1")
O6.B <- geeglm(NCalls ~ offset(LBroodSize) + FoodTreatment, data = Owls, family = poisson, id = NestNight, corstr = "ar1")


anova(O6, O6.A)
anova(O6, O6.B)

summary(O6)


# -->
# The correlation of the calls between two sequential arrivals is 0.51, which is relatively high.
# The overdispersion is 6.6, which is similar to that of the quasi-Poisson GLM.
# The estimated regression parameters are similar to those of the quasi-Poisson GLM, but the p-values are considerably larger (at least for the slopes).

# However, the biological conclusions are the same; there is a food treatment effect
# (lower number of calls from food satiated observations) and later the night, the less calls.



