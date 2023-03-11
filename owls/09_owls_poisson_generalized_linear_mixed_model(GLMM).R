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
# Poisson GLMM:  different intercept for each nest
# ------------------------------------------------------------------------------
library(nlme)
library(lme4)

# O1.lmer <- lmer(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent * ArrivalTime + (1 | fNest), data = Owls, family = poisson)
O1.lmer <- glmer(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent * ArrivalTime + (1 | fNest), data = Owls, family = poisson)

summary(O1.lmer)


# -->
# The correlation between the intercept and the slope for arrival time is rather large (-0.964).
# This is because arrival time was not centered.
# The model can be further simplified because the interaction between sex of the parent and arrival time is not significant.



# O2.lmer <- lmer(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent+ArrivalTime + (1 | fNest), data = Owls, family = poisson)
O2.lmer <- glmer(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent + ArrivalTime + (1 | fNest), data = Owls, family = poisson)

anova(O1.lmer, O2.lmer, test="F")

# O3.lmer <- lmer(NCalls ~ offset(LBroodSize) + FoodTreatment + ArrivalTime + (1 | fNest), data = Owls, family = poisson)
O3.lmer <- glmer(NCalls ~ offset(LBroodSize) + FoodTreatment + ArrivalTime + (1 | fNest), data = Owls, family = poisson)

anova(O1.lmer, O3.lmer, test="F")

summary(O3.lmer)



