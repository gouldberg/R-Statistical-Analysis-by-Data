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
# plot
#   (1) empirical logits and log odds ratio with linear line
#   (2) fitted logits and log odds ratio by quadratic binary logistic regression model
# ------------------------------------------------------------------------------

col <- c("blue", "red", "black")

pch <- c(15, 17, 16)

age <- coalminers$age



# -----------
grpahics.off()

par(mfrow = c(1,2))



# empirical logits and log odds ratio with linear line

matplot(age, logitsCM, type = "p",
        col = col, pch = pch, cex = 1.2, cex.lab = 1.25,
        xlab = "Age", ylab = "Log Odds or Odds Ratio", main = "empirical")

abline(lm(logitsCM[,1] ~ age), col = col[1], lwd = 2)
abline(lm(logitsCM[,2] ~ age), col = col[2], lwd = 2)
abline(lm(logitsCM[,3] ~ age), col = col[3], lwd = 2)


probs <- c(.01, .05, .10, .25, .5)

axis(4, at = qlogis(probs), labels = probs)

mtext("Probability", side = 4, cex = 1.2, at = -2, line = 2.5)

text(age[2], logitsCM[2,1] + .5, "Breathlessness", col = col[1], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2,2] + .5, "Wheeze", col = col[2], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2,3] - .5, "log OR\n(B|W)/(B|w)", col = col[3], pos = 1, cex = 1.2)




# ----------
# fitted logits and log odds ratio (quadratic binary logistic regression model)


# logits for B and W and log odds ratio
logitsP2 <- blogits(fitted(cm.vglm2)[,4:1])
logitsY2 <- blogits(depvar(cm.vglm2)[,4:1])


matplot(age, logitsCM, type = "p",
        col = col, pch = pch, cex = 1.2, cex.lab = 1.25,
        xlab = "Age", ylab = "Log Odds or Odds Ratio", main = "fitted")

lines(age, logitsP2[,1], col = col[1], lwd = 2)
lines(age, logitsP2[,2], col = col[2], lwd = 2)
lines(age, logitsP2[,3], col = col[3], lwd = 2)


probs <- c(.01, .05, .10, .25, .5)

axis(4, at = qlogis(probs), labels = probs)

mtext("Probability", side = 4, cex = 1.2, at = -2, line = 2.5)

text(age[2], logitsP2[2,1] + .5, "Breathlessness", col = col[1], pos = NULL, cex = 1.2)
text(age[2], logitsP2[2,2] + .5, "Wheeze", col = col[2], pos = NULL, cex = 1.2)
text(age[2], logitsP2[2,3] - .5, "log OR\n(B|W)/(B|w)", col = col[3], pos = 1, cex = 1.2)

