# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)



# ---------------------------------------------------------------------------
# Bayesian ANOVA:  calculate Bayes Factors and Ratio of the Bayes Factors
# ---------------------------------------------------------------------------

# In particular, BayesFactor has sensible defaults for weakly informative prior probabilities and makes model comparison easy
library(BayesFactor)


# ----------
set.seed(96761)

seg.bf1 <- lmBF(income ~ Segment, data = seg.df)
seg.bf2 <- lmBF(income ~ Segment + ownHome, data = seg.df)


summary(seg.bf1)
summary(seg.bf2)



# ----------
# Model comparison is performed by using the "/" operator to find the ratio of the models' Bayes Factors

seg.bf1 / seg.bf2



# -->
# The ratio of Bayes Factors for model 1 (~ Segment) vs. model 2 (~ Segment + ownHome) is 6.58
# This means that model 1 is the preferable model by a factor of 6.5



# ---------------------------------------------------------------------------
# Bayesian ANOVA:  Inspect model parameters
# ---------------------------------------------------------------------------
# Find the model parameters and their credible ranges
seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000)


# We care about six parameters: population mean, variance (mu and sigma) and the estimates of means for the four segments
plot(seg.bf.chain[,1:6])



# ----------
# Inspection the Posterior Draws
summary(seg.bf.chain)


# -->
# Note that the model estimates an overall mu that is the best guesssfor the population mean regardless of segment effects,
# and then estimates each segment as a deviation from that.
# However, for many purposes, it is more useful to have direct estimates for the mean of each segment rather than its deviation.
# To estimate the direct values for each segment, we add the population value (mu) to the deviations for each segment.

# We can not simply do that with the aggregate numbers here by adding the mu row to each of the other rows.
# Because the best estimates of segment totals are found within each draw. We need to compute segment values at that level and then
# summarize those estimates



# ---------------------------------------------------------------------------
# Calculate confidence interval
# ---------------------------------------------------------------------------
head(seg.bf.chain)


seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]

seg.bf.ci <- t(apply(seg.bf.chain.total, 2, quantile, pr = c(0.025, 0.5, 0.975)))

seg.bf.ci



# ----------
library(ggplot2)

seg.bf.df <- data.frame(seg.bf.ci)

seg.bf.df$Segment <- rownames(seg.bf.df)

p <- ggplot(seg.bf.df, aes(x = Segment, y = X50., ymax = X97.5., ymin = X2.5.))

p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab("Income") + ggtitle("95% CI for Mean Income by Segment") + coord_flip()







