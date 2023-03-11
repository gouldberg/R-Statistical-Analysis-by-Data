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
# Model prediction performance
#
# mlogit cannot use predict ??
# ------------------------------------------------------------------------------

x1 <- data3$price.heinz

x2 <- data3$price.hunts



# ----------
# Heinz, Hunts choice probability
data3$p1 <-exp(coef2*x1)/(exp(coef2*x1)+exp(coef1+coef2*x2))

data3$p2 <-exp(coef1+coef2*x2)/(exp(coef2*x1)+exp(coef1+coef2*x2))


data3 <- data3 %>% mutate(
  pred.choice = ifelse(p1 > 0.5, "heinz", "hunts"),
  hantei = ifelse(Ketchup.choice == pred.choice, 1, 0)
)



# ----------
# confusion matrix and accuracy
pred_tab <- xtabs(cnt ~ Ketchup.choice + pred.choice, data=data3)

sum(diag(pred_tab)) / sum(pred_tab)



