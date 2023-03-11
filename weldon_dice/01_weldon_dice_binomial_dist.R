setwd("//media//kswada//MyFiles//R//weldon_dice")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Weldon's dice
#  - W. F. Raphael Weldon, an English evolutionary biologist and joint founding editor of Biometrika, tallied the resutls of
#   throwing 12 dice 26,306 times.
#   For his purposes, he considered the outcome of 5 or 6 pips showing on each die to be a success and all other outcomes as failures.
#   Weldon reported his results in a ltter to Francis Galton dated February 2, 1984, in order
#   "to judge whether the differences between a series of group frequencies and a theoretical law .. were more than might be attributed
#   to the chance fluctuations of random sampling"
#   In terms of the number of occurrences of a 5 or 6 in the throw of 12 dice. If the dice were all identical and perfectly fair (balanced),
#   one would expect that p = Pr{5 or 6} = 1/3 and the distribution of the number of 5 or 6 would be binomial.
# ------------------------------------------------------------------------------
data("WeldonDice", package = "vcd")

data <- WeldonDice

data
sum(data)



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------
dimnames(data)$n56[11] <- "10+"

k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of 5s and 6s", ylab = "Frequency", col = "lightblue", cex.lab = 1.5)



# ------------------------------------------------------------------------------
# Calculate expected counts and Chisq raw residuals
# ------------------------------------------------------------------------------
data_df <- as.data.frame(data)

k <- 0:12
Pk <- dbinom(k, 12, 1/3)
Pk <- c(Pk[1:10], sum(Pk[11:13]))
Exp <- round(sum(data) * Pk, 5)
Diff <- data_df$Freq - Exp

Chisq <- Diff^2 / Exp
data.frame(data_df, Prob = round(Pk, 5), Exp, Diff, Chisq)


