setwd("//media//kswada//MyFiles//R//nes96")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nes96
# ------------------------------------------------------------------------------

data("nes96", package = "faraway")

str(nes96)

head(nes96)



# ----------
party <- nes96$PID

levels(party)


# We collapse this to three
levels(party) <- c("Democrat", "Democrat", "Independent", "Independent", "Independent", "Republican", "Republican")


# The income variable in the original data was an ordered factor with income ranges.
# We have converted this to a numeric variable by taking the midpoint of each range.
inca <- c(1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115)

income <- inca[unclass(nes96$income)]



# For simplicity, we consider only the age, education level and income group of the respondents
rnes96 <- data.frame(party, income, education = nes96$educ, age = nes96$age)


car::some(rnes96)



# ------------------------------------------------------------------------------
# Linear Discriminant Analysis
# ------------------------------------------------------------------------------

library(MASS)

mlda0 <- lda(party ~ age + education + income, data = rnes96)


mlda0



# -->
# The means of the group reveal that there is not much difference in the ages of the three groups,
# but there are noticeable differences in income.



# ----------
mlda <- update(mlda0, . ~ . - education)

mlda



# -->
# Trace:  We see that the 1st component is strongly dominant and so the classification will depend mostly on this.

# Coefficients of Linear Discriminants:
# We see that the 1st combination is dominated by the income while the 2nd by the age.
# However, the second combinations counts for little so the classification is based mainly on the income.



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

preds <- predict(mlda)

head(preds$posterior)



# ----------
# We can get the most likely outcome from each case and compare it against the observed class.
xtabs(~ predict(mlda)$class + rnes96$party)


# -->
# The result is quite similar to the multinominal logit model.



