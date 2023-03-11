setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


Accident$result <- factor(Accident$result, levels = c("Injured", "Died"))

str(Accident)



# ------------------------------------------------------------------------------
# Fit all 2-way interactions model
# ------------------------------------------------------------------------------

acci.mod2 <- glm(result ~ (age + mode + gender)^2, weight = Freq, data = Accident, family = binomial)


summary(acci.mod2)



# ----------
anova(acci.mod2, test = "Chisq")

car::Anova(acci.mod2)




# ----------
anova(acci.mod2, acci.mod)


LRstats(acci.mod2, acci.mod)



# ------------------------------------------------------------------------------
# Fit 3-way interactions model
# ------------------------------------------------------------------------------

acci.mod3 <- glm(result ~ age * mode * gender, weight = Freq, data = Accident, family = binomial)


summary(acci.mod3)



# ----------
anova(acci.mod3, test = "Chisq")

car::Anova(acci.mod3)




# ----------
anova(acci.mod, acci.mod2, acci.mod3)


LRstats(acci.mod, acci.mod2, acci.mod3)



# -->
# 3-way interactions are not significant.




