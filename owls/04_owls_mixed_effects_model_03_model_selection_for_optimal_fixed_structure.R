# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ----------
Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)

M1.lme <- lme(Form, random = ~ 1 | Nest, method = "REML", data = Owls)

M2.lme <- lme(Form, random = ~ 1 + ArrivalTime | Nest, method = "REML", data = Owls)



# ------------------------------------------------------------------------------
# Inspect the significance of the regression parameters
# ------------------------------------------------------------------------------

summary(M1.lme)



# -->
# Neither interaction term is significant.
# We could drop the least signiicant term and reapply the model.



# Note that you should NOT use the anova() here
# as it applies sequential testing. (which depends on the order of the 2-way interaction terms)
anova(M1.lme)




# ------------------------------------------------------------------------------
# model selection by Likelihood ratio test
# ------------------------------------------------------------------------------

M1.Full <- lme(Form, random = ~ 1 | Nest, method = "ML", data = Owls)

M1.A <- update(M1.Full, . ~ . - SexParent:FoodTreatment)

M1.B <- update(M1.Full, . ~ . - SexParent:ArrivalTime)


anova(M1.Full, M1.A)

anova(M1.Full, M1.B)



# -->
# Both interaction terms can be dropped from the model.



# ----------
Form2 <- formula(LogNeg ~ SexParent + FoodTreatment + SexParent * ArrivalTime)

M2.Full <- lme(Form2, random= ~ 1 | Nest, method = "ML", data = Owls)

M2.A <- update(M2.Full, . ~ . - FoodTreatment)

M2.B <- update(M2.Full, . ~ . - SexParent:ArrivalTime)

anova(M2.Full, M2.A)

anova(M2.Full, M2.B)


# -->
# The interaction term sex-arrival time is not significant so this was also ommitted.



# ----------
Form3 <- formula(LogNeg ~ SexParent + FoodTreatment + ArrivalTime)

M3.Full <- lme(Form3, random= ~ 1 | Nest, method = "ML", data = Owls)

M3.A <- update(M3.Full, . ~ . - FoodTreatment)

M3.B <- update(M3.Full, . ~ . - SexParent)

M3.C <- update(M3.Full, . ~ . - ArrivalTime)

anova(M3.Full, M3.A)

anova(M3.Full, M3.B)

anova(M3.Full, M3.C)


# -->
# The term sex of the parent is not significant.



# ----------
Form4 <- formula(LogNeg ~ FoodTreatment + ArrivalTime)

M4.Full <- lme(Form4, random = ~1 | Nest, method = "ML", data = Owls)

M4.A <- update(M4.Full, . ~ . - FoodTreatment)

M4.B <- update(M4.Full, . ~ . - ArrivalTime)

anova(M4.Full, M4.A)

anova(M4.Full, M4.B)



# -->
# Both food treatment and arrival time are significant at the 5% level.



# ------------------------------------------------------------------------------
# refit with REML and validate the model
# ------------------------------------------------------------------------------

M5 <- lme(LogNeg ~ FoodTreatment + ArrivalTime, random= ~ 1 | Nest, method = "REML", data = Owls)

summary(M5)



# -->
# The slope for food treatment is -0.175, meaning that sibling negotiation for an observation from an owl that was food satiated is -0.175 (on the lo-10 scale)
# than a food deprived sibling.
# Indicating that siblings are quieter if they have more food.
# The slope for arriva time is -0.03, which means that the later in the night the parents arrive, the lower the level of sibling negotiation.


# The random intercept is normally distributed with mean 0 and variance 0.09^2.
# The residual term is normally distributed with mean 0 and variance 0.23^2

# The correlation between observations from the same nest = 0.13
# This is relatively low, but significant
0.09^2 / (0.09^2 + 0.23^2)


# Note the there is high correlation between the intercept and the slope for arrival.
# This is because all arrival values are between 22 and 30 (recall that 30 is 06.00AM).
# The intercept is obviously far outside the range of the sampled arrival time values.
# A small change in the slope can therefore have a large change on the intercept, hence the high correlation.


# -->
# It would be better to centre arrival time around 0


