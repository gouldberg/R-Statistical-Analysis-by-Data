setwd("//media//kswada//MyFiles//R//birthwt")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birthwt
#   - Data on 189 babies born at Baystate Medical Center, Springfield, MA during 1986.
#   - The quantitative response is bwt (birth weight in grams), and this is also recoded as low, a binary variable corresponding to bwt < 2500 (2.5 kg).
# ------------------------------------------------------------------------------
data("birthwt", package = "MASS")

data <- birthwt

dim(data)
str(data)


# ----------
Hmisc::describe(data)



# ----------
data$race <- factor(data$race, labels = c("white", "black", "other"))
data$ptd <- factor(data$ptl > 0)  # premature labors
data$ftv <- factor(data$ftv)  # physician visits
levels(data$ftv)[-(1:2)] <- "2+"
data$smoke <- factor(data$smoke > 0)
data$ht <- factor(data$ht > 0)
data$ui <- factor(data$ui > 0)

data$low <- factor(data$low, levels = c(0, 1))
data$bwt <- NULL
data$ptl <- NULL


btw.full <- glm(low ~ ., data = data, family = binomial)



# ------------------------------------------------------------------------------
# Stepwise variable selection
# ------------------------------------------------------------------------------
library(MASS)

btw.step1 <- stepAIC(btw.full, trace = TRUE, direction = "both")

btw.step1$anova



# ---------
# Alternatively we can use the BIC criterion, which generally will select a smaller model when the sample size is reasonably large
btw.step2 <- stepAIC(btw.full, trace = TRUE, direction = "both", k = log(nrow(data)))

btw.step2$anova



# ----------
# estimated coefficients (Wald test)
lmtest::coeftest(btw.step1)
lmtest::coeftest(btw.step2)



# Type II tests of each effect
car::Anova(btw.step1)
car::Anova(btw.step2)



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------
# Two models are nested. Likelihood ratio test can be applied  --> larger model is significantly better.
anova(btw.step2, btw.step1, test="Chisq")



# ------------------------------------------------------------------------------
# Allow a nonlinear term (natural cubis splines)
# and all 2-way interactions of the binary predictors
# ------------------------------------------------------------------------------
btw.glm3 <- update(btw.step1, . ~ . + ns(lwt,2))
btw.step3 <- stepAIC(btw.glm3, trace = TRUE, direction = "both")


# btw.glm4 <- update(btw.step1, . ~ . + age * race)
# btw.step4 <- stepAIC(btw.glm4, trace = TRUE, direction = "both")

lmtest::coeftest(btw.step3)
# lmtest::coeftest(btw.step4)



# ----------
anova(btw.step1, btw.step3, test = "Chisq")


# --> 
# None of these additional terms have effect


