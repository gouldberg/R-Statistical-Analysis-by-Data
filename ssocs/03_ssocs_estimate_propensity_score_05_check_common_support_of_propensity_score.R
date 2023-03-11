setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Summary statistics of propensity scores
# ------------------------------------------------------------------------------

# logistic regression 
with(SSOCS.data, by(pScores, treat, summary))


# -->
# The maximum propensity socres of schools with and without a security employee are similar,
# which is preliminary evidence of adequate common support.


# GBM
with(SSOCS.data, by(pScoresGBM, treat, summary))



# for all models
by(SSOCS.data[,c("pScores", "pScoresGBM")], SSOCS.data$treat, summary)



# ----------
# create a table
tableCommonSupport = rbind(
  summary(SSOCS.data[SSOCS.data$treat == 1, c("pScores", "pScoresGBM")]),
  summary(SSOCS.data[SSOCS.data$treat == 0, c("pScores", "pScoresGBM")]))

rownames(tableCommonSupport) = c(rep("Treated",6), rep("Control",6))

tableCommonSupport



# write.csv(tableCommonSupport, file="Table_common_support.csv")   



# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# ------------------------------------------------------------------------------

# obtain proportion of treated cases above maximum control cases and proportion of control cases below minum treated cases
# for logistic regression

with(SSOCS.data, 100*c(
  mean(as.numeric(pScores[treat==1] > max(pScores[treat==0]))),
  mean(as.numeric(pScores[treat==0] < min(pScores[treat==1])))))



# obtain proportions of treated cases above maximum control cases
with(SSOCS.data, 100*c(
  mean(as.numeric(pScores[treat==1] > max(pScores[treat==0]))),
  mean(as.numeric(pScoresGBM[treat==1] > max(pScoresGBM[treat==0])))))



# obtain proportions of control cases below minimum treated cases
with(SSOCS.data, 100*c(
  mean(as.numeric(pScores[treat==0] < min(pScores[treat==1]))),
  mean(as.numeric(pScoresGBM[treat==0] < min(pScoresGBM[treat==1])))))




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Box-and-Whiskers plot
# ------------------------------------------------------------------------------

# evaluate common support with box and whiskers plot
SSOCS.data$treat = factor(SSOCS.data$treat)


# ----------
library(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))


bwplot(pScores ~ treat, data = SSOCS.data, ylab = "Propensity Scores", xlab = "Treatment",auto.key = TRUE)
bwplot(pScoresGBM ~ treat, data = SSOCS.data,  ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE)



# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# histogram
# ------------------------------------------------------------------------------

hist(SSOCS.data$pScores[SSOCS.data$treat==0], density = 10, angle = 45, main="Propensity Scores", xlab="Shaded = Untreated | Gray = Treated") 
hist(SSOCS.data$pScores[SSOCS.data$treat==1], col=gray(0.4,0.25), add=T) 


hist(SSOCS.data$pScoresGBM[SSOCS.data$treat==0], density = 10, angle = 45, main="Propensity Scores", xlab="Shaded = Untreated | Gray = Treated") 
hist(SSOCS.data$pScoresGBM[SSOCS.data$treat==1], col=gray(0.4,0.25), add=T) 



# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Kernel density plots
# ------------------------------------------------------------------------------

require(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))

densityplot( ~pScores, groups = treat, plot.points = F, xlim = c(0,1), lwd = 2,
             data = SSOCS.data,  ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE)


densityplot( ~pScoresGBM, groups = treat, plot.points = F, xlim = c(0,1), lwd = 2,
             data = SSOCS.data,  ylab = "Propensity Scores", xlab = "Treatment", auto.key = TRUE)


