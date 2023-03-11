setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  email
# ------------------------------------------------------------------------------

email <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

str(email)

dim(email)


car::some(email)




# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained (NagelKerke R^2 and McFadden's pseudo R^2)
# ------------------------------------------------------------------------------


mod_list <- list(ps_model_ori, ps_model_biased)

mod_name <- c("ps_model_ori", "ps_model_biased")



for(i in 1:length(mod_list)){
  mod <- mod_list[[i]]
  mod_n <- mod_name[i]
  
  
  # Nagelkerke R^2
  n <- mod$df.null + 1
  
  nr2 <- round((1 - exp((mod$dev - mod$null) / n)) / (1 - exp(-mod$df.null/n)), 3)
  
  
  # ----------
  # McFadden7s peudo R^2
  
  mr2 <- round(1 - mod$deviance / mod$null.deviance, 3)
  
  
  print(paste0("model name   : ", mod_n))
  print(paste0("Nagelkerke R2: ", nr2))
  print(paste0("McFadden R2  : ", mr2))
}



# -->
# very poor fit ..



# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------


library(ResourceSelection)


for(i in 1:length(mod_list)){
  
  mod <- mod_list[[i]]
  mod_n <- mod_name[i]
  
  # g = 10: number of bins to use to calculate quantiles
  hl <- hoslem.test(mod$y, fitted(mod), g = 10)
  
  print(paste0("model name   : ", mod_n))
  print(paste0("Hosmer - Lemeshow Test pvalue: ", round(hl$p.value, 3)))
}




# -----------
# Observed vs. Expected

cbind(hl$observed, hl$Expected)





# ------------------------------------------------------------------------------
# Squared Pearson Correlation Coefficients of observed outcome with the estimated probability
# ------------------------------------------------------------------------------


pred <- predict(ps_model_biased, newdata = biased_data, type = "response")


cor((biased_data$treat == 1) * 1, pred)^2

