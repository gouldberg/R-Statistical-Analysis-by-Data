# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Assess covariate balance as baseline and after stratification
# ------------------------------------------------------------------------------


covariateNames <- c("recency", "history", "channel")


balanceTable <- data.frame()


for (var in 1:length(covariateNames)) {
  
  formula_base <- paste("treatment ~ ", covariateNames[var], sep = "")
  formula_balance <- paste("treatment ~ strataGPS + ", covariateNames[var], sep = "")
  
  
  # ----------
  # regress dose on covariate without weights
  # -(1:5):  remove intercept and strataGPS2 - 5
  # for dummy-coded categorical covariates, the largest coefficient of the dummy codes is selected
  
  maxEff_base <- max(abs(coef(lm(formula_base, biased_data))[-(1)]))
  maxEff_balance <- max(abs(coef(lm(formula_balance, biased_data))[-(1:5)]))
  
  balanceTable <- rbind(balanceTable, c(var, maxEff_base, maxEff_balance))
}


names(balanceTable) <- c("variable", "coef_base", "coef_balance") 

balanceTable$variable <- covariateNames




# ----------
# standardize coefficients with respect to sd of outcome

balanceTable$coef_base <- round(balanceTable$coef_base / sd(biased_data$treatment), 4)

balanceTable$coef_balance <- round(balanceTable$coef_balance / sd(biased_data$treatment), 4)


balanceTable




# ----------
balanceTable %>% filter(coef_balance > 0.1)




# -->
# Covariate balance is considered adequate if the standardized regression coefficient is lower than 0.1,
# but guidelines for covariate balance evaluation for propensity score analysis with treatment does
# are not yet well established.

# covariate balance of channel is improved ..


