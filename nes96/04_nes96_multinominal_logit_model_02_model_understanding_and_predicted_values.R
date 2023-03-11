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
# Model performance:  Accuracy
# ------------------------------------------------------------------------------


xtabs(~ predict(mmodi) + rnes96$party)


# Compute the proportion correctly classified as
(284 + 0 + 159) / nrow(rnes96)



# -->
# We see that only 47% of the current data are correctly classified and we could expect the performance on new individuals to be slightly worse than this.
# No cases are classified as independnts because the probability of the other two outcomes always dominates this outcome.
# We see that the majority of actual Republicans are classified as Democrats.

# The multinomial logit model is not usually the best choice for classification performance.
# Better performance can be obtained by methods specialized for this purpose such as random forests or support vector machines.



# ------------------------------------------------------------------------------
# Model understanding
# ------------------------------------------------------------------------------

summary(mmodi)



# ----------
# The intercept terms model the probabilities of the party identification for an income of zero.

cc <- c(0, -1.17493, -0.95036)

exp(cc) / sum(exp(cc))


predict(mmodi, data.frame(income = 0), type = "probs")




# ----------
# The slope terms represent the log-odds of moving from the baseline category of Democrat to Independent and Republican,
# respectively, for a unit change of $1000 in income

( pp <- predict(mmodi, data.frame(income = c(0, 1)), type = "probs") )

log(pp[1,1] * pp[2,2] / (pp[1,2] * pp[2,1]))

log(pp[1,1] * pp[2,3] / (pp[1,3] * pp[2,1]))



# ------------------------------------------------------------------------------
# Predicted values
# ------------------------------------------------------------------------------

inclevels <- 0:110


preds <- data.frame(income = inclevels, predict(mmodi, data.frame(income = inclevels), type = "probs"))


# ----------
library(tidyr)

lpred <- gather(preds, party, probability, -income)

ggplot(lpred, aes(x = income, y = probability, group = party, linetype = party)) + geom_line()


