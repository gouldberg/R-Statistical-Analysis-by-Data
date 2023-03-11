setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# Calculate bivariate logits and logg odds ratio
# ------------------------------------------------------------------------------

# convert it to a 4 * 9 data frame, adn relable the columns to use the combinations of "B", "b", "W", "w"
# to represent the conditions of breathlessness and wheeze,
# where the upper case letter indicates presence of the condition

coalminers <- data.frame(t(matrix(aperm(CoalMiners, c(2, 1, 3)), 4, 9)))


colnames(coalminers) <- c("BW", "Bw", "bW", "bw")


coalminers$age <- c(22, 27, 32, 37, 42, 47, 52, 57, 62)


coalminers




# ----------

library(vcdExtra)


# blogits() calculates the bivariate logits and log odds ratio
logitsCM <- blogits(coalminers[,1:4], add = 0.5)


colnames(logitsCM)[1:2] <- c("logitB", "logitW")


logitsCM
                   
                   


# ------------------------------------------------------------------------------
# plot empirical logits and log odds ratio
# ------------------------------------------------------------------------------

col <- c("blue", "red", "black")

pch <- c(15, 17, 16)

age <- coalminers$age



# -----------
par(mar = c(4, 4, 1, 4) + .2)

matplot(age, logitsCM, type = "p",
        col = col, pch = pch, cex = 1.2, cex.lab = 1.25,
        xlab = "Age", ylab = "Log Odds or Odds Ratio")

abline(lm(logitsCM[,1] ~ age), col = col[1], lwd = 2)
abline(lm(logitsCM[,2] ~ age), col = col[2], lwd = 2)
abline(lm(logitsCM[,3] ~ age), col = col[3], lwd = 2)



# ----------
# right probability axis (equivalent probabilities for the logits)

probs <- c(.01, .05, .10, .25, .5)

axis(4, at = qlogis(probs), labels = probs)

mtext("Probability", side = 4, cex = 1.2, at = -2, line = 2.5)

text(age[2], logitsCM[2,1] + .5, "Breathlessness", col = col[1], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2,2] + .5, "Wheeze", col = col[2], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2,3] - .5, "log OR\n(B|W)/(B|w)", col = col[3], pos = 1, cex = 1.2)




# -->
# We see that both symptoms, while quite rare among young miners, increase steadily with age (or years working in the mine).
# By age 60, the probability is nearly 0.5 of having either condition.
# There is a hint of curvilinearity, particularly in the logit for breathlessness.
# The decline in the odds ratio with age may reflect selection, as miners who had retired for health or other reasons
# were excluded from the study.


