setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))

fam <- data$family
levels(fam)[c(3,4,6,7,9)] <- "Others"

fam <- factor(fam, levels(fam)[c(1, 2, 4:6, 3)])
data$family <- fam



# ------------------------------------------------------------------------------
# Logistic regression models: non-linear mode
#  - linear model (sex and age effects are addtive)
#  - linear model with interaction (not additive)
# ------------------------------------------------------------------------------
donner.mod1 <- glm(survived ~ age + sex, data = data, family = binomial)
donner.mod2 <- glm(survived ~ age * sex, data = data, family = binomial)



# ----------
# car::Anova() provide Type II tests of each effect
# The main effects of age and sex are both significant, but the interaction term, age:sex, is not in model donner.mod2.
car::Anova(donner.mod1)
car::Anova(donner.mod2)



# ------------------------------------------------------------------------------
# Logistic regression models: non-linear (quadratic, here) model (the effect is quadratic on the logit scale)
#  - non linear model
#  - non linear model with interaction
# ------------------------------------------------------------------------------
donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = data, family = binomial)
donner.mod4 <- glm(survived ~ poly(age, 2) * sex, data = data, family = binomial)


# Now, in model donner.mod4, the interaction term poly(age,2):sex is significant, indicating that the fitted quadratics for males and females differ in shape,
# meaning either their linear (slope) or quadratic (curvature) components.
car::Anova(donner.mod3)
car::Anova(donner.mod4)



# ------------------------------------------------------------------------------
# Compare 4 models by AIC and BIC, and residual deviance
# ------------------------------------------------------------------------------
# By AIC and BIC, donner.mod4 is best, and it is also the only model with a non-significant LR X^2 (residual deviance).
vcdExtra::LRstats(donner.mod1, donner.mod2, donner.mod3, donner.mod4)



mods <- list(donner.mod1, donner.mod2, donner.mod3, donner.mod4)
LR <- sapply(mods, function(x) x$deviance)
LR <- matrix(LR, 2, 2)
rownames(LR) <- c("additive", "non-add")
colnames(LR) <- c("linear", "non-lin")
LR <- cbind(LR, diff = LR[,1] - LR[,2])
LR <- rbind(LR, diff = c(LR[1,1:2] - LR[2,1:2], NA))
LR


# --> There is evidence that the relationship of survival to age differs for men and women in the Donner Party.
# These relationships are not well-described by a linear logistic regression.



# ------------------------------------------------------------------------------
# Logistic regression models: non-linear by splines model
#
#  - ns() method constrains the fitted cubic spline to be linear at lower and upper limits of x, and, for k knots, fits df = k + 1 parameters
#    not counting the intercept.  The k knots can be conveniently chosen as k cutpoints in the percentiles of the distribution of x.
# ------------------------------------------------------------------------------
# We fit two natural spline modes models with 2 and 4 df, and compare these with the quadratic model (donner.mod4)
library(splines)
donner.mod5 <- glm(survived ~ ns(age,2) * sex, data = data, family = binomial)
donner.mod6 <- glm(survived ~ ns(age,4) * sex, data = data, family = binomial)

car::Anova(donner.mod5)
car::Anova(donner.mod6)



attr(ns(data$age,4), "knots")

head(data$age)
head(ns(data$age,4))



# ----------
# With four more parameters, donner.mod6 fits better and has a smaller AIC.
vcdExtra::LRstats(donner.mod4, donner.mod5, donner.mod6)



# ----------
# Effect plot for the spline model donner.mod6.
# Note that the fitted effects are plotted on the logit scale and labeled with the corresponding probabilities,
# whereas the conditional plots are plotted directly on the probability scale.
donner.eff6 <- effects::allEffects(donner.mod6, xlevels = list(age=seq(0, 50, 5)))
plot(donner.eff6, ticks = list(at=c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)))


# --> For women in the Donner Party, survival was greatest for those aged 10-30.
# Survival among men was overall much less and there is a hint of greater survival for men aged 10-15.



