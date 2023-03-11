# setwd("//media//kswada//MyFiles//R//els_career_academy")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\els_career_academy")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# load results with imputed and estimated propensity scores
# ------------------------------------------------------------------------------

load("imputed_and_estimated_propensity_score.Rdata")




# ------------------------------------------------------------------------------
# Summary statistics of propensity scores
# estimated with the first imuputed dataset
# ------------------------------------------------------------------------------

# logistic regression 
with(ELS.data.imputed, by(pScores, treat, summary))


# random forest with conditional inference scheme
with(ELS.data.imputed, by(pScoresRf, treat, summary))


# GBM
with(ELS.data.imputed, by(pScoresGBM, treat, summary))



# for all models
by(ELS.data.imputed[,63:65], ELS.data.imputed$treat, summary)



# ----------
# create a table
tableCommonSupport = rbind(
  summary(ELS.data.imputed[ELS.data.imputed$treat == 1, 63:65]),
  summary(ELS.data.imputed[ELS.data.imputed$treat == 0, 63:65]))

rownames(tableCommonSupport) = c(rep("Treated",6), rep("Control",6))

tableCommonSupport



# write.csv(tableCommonSupport, file="Table_common_support.csv")   




# ------------------------------------------------------------------------------
# AUC and ROC: distance's power to discriminate treat or not
# ------------------------------------------------------------------------------

library(ROCR)


# AUC
performance(prediction(ELS.data.imputed$pScores, ELS.data.imputed$treat), measure = "auc")@y.values[[1]]



# -->
# AUC is 0.719 < 0.8



# ----------
par(mfrow = c(1,1))

perf <- performance(prediction(ELS.data.imputed$pScores, ELS.data.imputed$treat), "tpr", "fpr")

plot(perf, col = "blue")

abline(a = 0, b = 1)




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# ------------------------------------------------------------------------------

# obtain proportion of treated cases above maximum control cases and proportion of control cases below minum treated cases
# for logistic regression

with(ELS.data.imputed, 100*c(
  mean(as.numeric(pScores[treat==1] > max(pScores[treat==0]))),
  mean(as.numeric(pScores[treat==0] < min(pScores[treat==1])))))



# obtain proportions of treated cases above maximum control cases
with(ELS.data.imputed, 100*c(
  mean(as.numeric(pScores[treat==1] > max(pScores[treat==0]))),
  mean(as.numeric(pScoresRf[treat==1] > max(pScoresRf[treat==0]))),
  mean(as.numeric(pScoresGBM[treat==1] > max(pScoresGBM[treat==0])))))



# obtain proportions of control cases below minimum treated cases
with(ELS.data.imputed, 100*c(
  mean(as.numeric(pScores[treat==0] < min(pScores[treat==1]))),
  mean(as.numeric(pScoresRf[treat==0] < min(pScoresRf[treat==1]))),
  mean(as.numeric(pScoresGBM[treat==0] < min(pScoresGBM[treat==1])))))




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Box-and-Whiskers plot
# ------------------------------------------------------------------------------

# evaluate common support with box and whiskers plot
ELS.data.imputed$treat = factor(ELS.data.imputed$treat)


# ----------
library(lattice)
# lattice.options(default.theme = standard.theme(color = FALSE))
# bwplot(pScores ~ treat, data = ELS.data.imputed, ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE, main = "by logistic regression")
# bwplot(pScoresRf ~ treat, data = ELS.data.imputed, ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE, main = "by random forest")
# bwplot(pScoresGBM ~ treat, data = ELS.data.imputed,  ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE, main = "by GBM")



par(mfrow = c(2,2))

boxplot(pScores ~ treat, data = ELS.data.imputed, ylab = "Propensity Scores", xlab = "Treatment", main = "by logistic regression", ylim = c(0, 1))

boxplot(pScoresRf ~ treat, data = ELS.data.imputed, ylab = "Propensity Scores", xlab = "Treatment", main = "by random forest", ylim = c(0, 1))

boxplot(pScoresGBM ~ treat, data = ELS.data.imputed,  ylab = "Propensity Scores", xlab = "Treatment", main = "by GBM", ylim = c(0, 1))




# -->
# Indicate that there is some lack of common support for propensity scores estimated with all three models,
# but the common support is best with the propensity scores estimated with logistic regression

# However, this conclusion is preliminary
# because the consequences of the observed common support depend on both the type of treatment effect of interest and
# the propensity score mothod used and cannot be fully understood until the propensity score method of interest is implemented.



# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Kernel density plots
# ------------------------------------------------------------------------------

require(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))

densityplot( ~pScores, groups=treat, plot.points=F, xlim=c(0,1), lwd=2,
             data = ELS.data.imputed,  ylab = "Propensity Scores", xlab = "Treatment",auto.key = TRUE)


densityplot( ~pScoresRf, groups=treat, plot.points=F, xlim=c(0,1), lwd=2,
             data = ELS.data.imputed,  ylab = "Propensity Scores", xlab = "Treatment",auto.key = TRUE)


densityplot( ~pScoresGBM, groups=treat, plot.points=F, xlim=c(0,1), lwd=2,
             data = ELS.data.imputed,  ylab = "Propensity Scores", xlab = "Treatment",auto.key = TRUE)




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# all imputed datasets
# ------------------------------------------------------------------------------

# obtain histograms of propensity scores estimated with logistic regression for all imputed datasets
# first stack the imputed datasets

allImputationsStacked <- data.frame() 


for (imp in 1:5) {
  temp <- cbind(allImputations$imputations[[imp]], imputation = imp)
  allImputationsStacked = rbind(allImputationsStacked, temp)
}


allImputationsStacked$treat <- factor(allImputationsStacked$treat, levels = c(0,1), labels=c("Untreated", "Treated"))


allImputationsStacked$imputation <- factor(allImputationsStacked$imputation, labels = paste("Imputation", 1:5))



# ----------
lattice.options(default.theme = standard.theme(color = FALSE))

densityplot(~ pScores | imputation, data = allImputationsStacked, plot.points = F, lwd = 2, groups = treat, xlab = "Propensity Scores", auto.key = TRUE)

bwplot(pScores ~ treat | imputation, data = allImputationsStacked, lwd = 2, ylab = "Propensity Scores", auto.key = TRUE)



