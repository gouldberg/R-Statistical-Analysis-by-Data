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



donner.mod1 <- glm(survived ~ age + sex, data = data, family = binomial)
donner.mod2 <- glm(survived ~ age * sex, data = data, family = binomial)
donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = data, family = binomial)
donner.mod4 <- glm(survived ~ poly(age, 2) * sex, data = data, family = binomial)

library(splines)
donner.mod5 <- glm(survived ~ ns(age,2) * sex, data = data, family = binomial)
donner.mod6 <- glm(survived ~ ns(age,4) * sex, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Examine Grayson's 1990 claim
#  - that survival in the Donner Party was also mediated by the size of the family unit
# ------------------------------------------------------------------------------
# Use last name for family
lname <- strsplit(rownames(data), ",")
lname <- sapply(lname, function(x) x[[1]])
data$family.size <- as.vector(table(lname)[lname])



# ----------
# Fit a new model that adds a main effect of family.size to donner.mod4 and donner.mod6, both include the interaction of age and sex and nonlinear terms in age
donner.mod7 <- glm(survived ~ poly(age, 2) * sex + family.size, data = data, family = binomial)
donner.mod8 <- glm(survived ~ ns(age,4) * sex + family.size, data = data, family = binomial)

car::Anova(donner.mod7)
car::Anova(donner.mod8)



# ----------
# donner.mod7 fits better and has a smaller AIC and BIC.
vcdExtra::LRstats(donner.mod4, donner.mod6, donner.mod7, donner.mod8)



# ----------
anova(donner.mod4, donner.mod7, test = "Chisq")



# ----------
# Effect plot
donner.eff7 <- effects::allEffects(donner.mod7, xlevels = list(age=seq(0, 50, 5)))
plot(donner.eff7, ticks = list(at=c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)))



# ----------
# crPlots() are NOT AVAILABLE for models with interactions
# car::crPlots(donner.mod7, ~family.size, id=TRUE)

