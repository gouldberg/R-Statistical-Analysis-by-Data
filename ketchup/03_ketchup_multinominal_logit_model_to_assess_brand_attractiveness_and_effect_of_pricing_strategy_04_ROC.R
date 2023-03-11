# setwd("//media//kswada//MyFiles//R//ketchup")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//ketchup")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Ketchup
# ------------------------------------------------------------------------------

data("Ketchup", package = "Ecdat")


dim(Ketchup)


str(Ketchup)


head(Ketchup)



# ----------
data <- Ketchup



# ------------------------------------------------------------------------------
# ROC and AUC
# ------------------------------------------------------------------------------

library(pROC)



# ----------
# ROC curve

ROC <- roc(response=data3$Ketchup.choice, predictor=data3$p2)

plot(1-ROC$specificities, ROC$sensitivities, xlab="1-Specificity", ylab="Sensitivity", type="l", lwd=2)

abline(a=0,b=1,lty=2)




# ----------
# AUC confidence interval

CI <- ci.auc(ROC, conf.level=0.95)



# ----------

cutoff <- coords(ROC, x="best", ret=c("threshold", "sensitivity", "specificity", "ppv", "npv"), best.method="closest.topleft")


# cut-off point in model
c.point <- cutoff[1]


beta <- coef(result)


# cut-off point in original data
cutoff.variable <- (log(c.point/(1-c.point))-beta[1])/beta[2]



# ----------

ROC$auc

CI

c.point

# when number of predictor is only 1
cutoff.variable

cutoff[2:5]



# sensitivity
# specificity
# ppv:  positive predictive value
# npv:  negative predictive value



