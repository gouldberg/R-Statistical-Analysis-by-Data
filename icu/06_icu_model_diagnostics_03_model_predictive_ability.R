setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
mod <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)

summary(mod)



# ------------------------------------------------------------------------------
# Model Predictive Ability:
# Histogram of probability by each class
# ------------------------------------------------------------------------------

par(mfrow=c(2,1))

pp_ps <- predict(mod, newdata = ICU2 %>% filter(died == "Yes"), type = "response")

pp_ng <- predict(mod, newdata = ICU2 %>% filter(died == "No"), type = "response")

hist(pp_ps, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Positive Class", xlab = "", col = "lightblue", ylim = c(0, 70))

hist(pp_ng, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Negative Class", xlab = "", col = "salmon", ylim = c(0, 70))




# ------------------------------------------------------------------------------
# Model Predictive Ability:
# ROC curve, Precision-Recall curve,  Sensitivity vs Specificity,  Lift chart
# ------------------------------------------------------------------------------

library(ROCR)

library(MLmetrics)

pp <- predict(mod, newdata = ICU2, type = "response")

ll <- as.numeric(ICU2$died) * 1 - 1


# data Conversion
pred <- prediction(predictions = pp, labels = ll)



# ----------
# ROC curve, Precision-Recall curve,  Sensitivity vs Specificity,  Lift chart
perf_roc <- performance(pred, "tpr", "fpr")

perf_pr <- performance(pred, "prec", "rec")

perf_ss <- performance(pred, "sens", "spec")

perf_lift <- performance(pred, "lift", "rpp")



# ----------
# plotting
par(mfrow=c(2,2))
plot(perf_roc, avg= "threshold", colorize=T, lwd= 3, main= "... ROC curve ...")
plot(perf_roc, lty=3, col="grey78", add=T)
plot(perf_pr, avg= "threshold", colorize=T, lwd= 3,main= "... Precision/Recall curve ...")
plot(perf_pr, lty=3, col="grey78", add=T)
plot(perf_ss, avg= "threshold", colorize=T, lwd= 3, main="... Sensitivity/Specificity plot ...")
plot(perf_ss, lty=3, col="grey78", add=T)
plot(perf_lift, avg= "threshold", colorize=T, lwd= 3, main= "... Lift chart ...")
plot(perf_lift, lty=3, col="grey78", add=T)



# ------------------------------------------------------------------------------
# Model Predictive Ability:
# AUC, F1, Gini, Logloss
# ------------------------------------------------------------------------------

performance(pred, measure = "auc")@y.values[[1]]

F1_Score(y_pred = (pp < 0.5)*1, y_true = ll, positive = "1")

Gini(y_pred = pp, y_true = ll)

LogLoss(y_pred = pp, y_true = ll)



# ------------------------------------------------------------------------------
# Model Predictive Ability:
# ROC curve with various type
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(perf_roc, colorize=T, lwd=2, main='ROC curves from 10-fold cross-validation')

plot(perf_roc, avg='vertical', spread.estimate='stderror',lwd=3,main='Vertical averaging + 1 standard error',col='blue')

plot(perf_roc, avg='horizontal', spread.estimate='boxplot',lwd=3,main='Horizontal averaging + boxplots',col='blue')

plot(perf_roc, avg='threshold', spread.estimate='stddev',lwd=2, main='Threshold averaging + 1 standard deviation',colorize=T)


