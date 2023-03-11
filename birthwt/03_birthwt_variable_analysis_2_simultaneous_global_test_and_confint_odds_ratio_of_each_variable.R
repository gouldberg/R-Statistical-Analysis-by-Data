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



# ------------------------------------------------------------------------------
# Simultaneous global test
# H0: beta = 0 that all regression coefs are zero
#
#  --> If this test is not significant, it makes little sense to use selection methods to choose individually significant predictors.
# ------------------------------------------------------------------------------
bwt.full <- glm(low ~ ., data = data, family = binomial)


# ----------
# lmtest::coeftest() is simple version of summary()
summary(bwt.full)
lmtest::coeftest(bwt.full)



# simultaneous global test of H0: beta = 0 that all regression coefs are zero
LRtest <- function(model) c(LRchisq = (model$null.deviance - model$deviance), df = (model$df.null - model$df.residual))
( LR <- LRtest(bwt.full) )
( pvalue <- 1 - pchisq(LR[1], LR[2]) )



# -->
# Simultaneous global test is significant, it makes sense to use selection methods to choose individually significant predictors



# ------------------------------------------------------------------------------
# Relative contribution and confidence interval of odds ratios for each variables
# ------------------------------------------------------------------------------
library("rms")

dd <- datadist(data);  options(datadist = "dd")

bwt.lrm1 <- lrm(low ~ ., data = data)

sum.lrm1 <- summary(bwt.lrm1)

bwt.lrm1


# ----------
# relative contribution
plot(anova(bwt.lrm1))



# ----------
# confidence interval of odds ratios for each variables
plot(sum.lrm1, log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))



