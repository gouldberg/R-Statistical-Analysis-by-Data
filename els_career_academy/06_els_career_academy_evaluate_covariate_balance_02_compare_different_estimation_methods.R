setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Compare covariate balance with different propensity estimation methods
# using weights for the ATT
# ------------------------------------------------------------------------------
# create a custom function to return balance summaries 
# the arguments are:
# data - the name of the data.frame containing the data.
# samplingWeight - the name of the variable containing the sampling weight, in quotes
# PSWeight - the name of the variable containing the propensity score weight, in quotes
# treatment - the of the treatment indicator variable, in quotes
# the type of treatment effect, either "ATT" or "ATE"
# covariateNames - the vector of covariate names


balanceSummarizer <- function(data, samplingWeight, PSWeight, treatment, effect, covariateNames) {
  
  finalWeight <- data[, samplingWeight] * data[, PSWeight]
  
  # evaluate covariate balance for ATT
  require(twang)
  balance.table <- bal.stat(data, vars= covariateNames, treat.var = treatment, 
                            w.all = finalWeight, get.ks = F, sampw = data[, samplingWeight], estimand = effect, multinom = F)
  
  balance.table <- balance.table$results 
  
  # calculate variance ratio
  balance.table$varRatio <- with(balance.table, tx.sd^2/ct.sd^2) 
  
  # summarize the covariate balance quality
  return(rbind(
    std.eff.sz = summary(abs(balance.table$std.eff.sz)), #standardized effect sizes
    varRatio = summary(balance.table$varRatio) )) # variance ratios
  
  #close function
}



# ----------
# obtain summaries of covariate balances with different propensity scores using the balanceSummarizer function I defined above

Table3.1 <- rbind(
  
  logistic = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATT", effect="ATT",
                               samplingWeight="bystuwt",  treatment="treat", covariateNames=covariateNames),
  
  RF = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATTRf", effect="ATT",
                         samplingWeight="bystuwt",  treatment="treat", covariateNames=covariateNames),
  
  GBM = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATTGBM",effect="ATT",
                          samplingWeight="bystuwt",  treatment="treat", covariateNames=covariateNames))


Table3.1 <- data.frame(round(Table3.1, digits = 4), index=rep(c("std.eff.sz","varRatio"), 3), method = rep(c("Logistic","RF","GBM"), each=2))


# ----------
# Summary of absolute standardized effect sizes with weights for the ATT
# --> Absolute standardized effect sizes below 0.1 standard deviations can be considered to indicate adequate covariate balance,
# but differences below 0.25 standard deviations could be acceptable
# if additional regression adjustment is performed.
# Weights from propensity scores estimated with logistic regression provided the best covariate balance
# among the methods used to estimate propensity scores and
# met the criteria for adequate balance for all covariates.

head(Table3.1)




# ------------------------------------------------------------------------------
# Compare covariate balance for estimating the ATE with different propensity estimation methods
# ------------------------------------------------------------------------------

Table3.2 <- rbind(
  logistic = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATE", effect="ATE",
                               samplingWeight="bystuwt", treatment="treat", covariateNames=covariateNames),

  truncated = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATETruncated", effect="ATE",
                                samplingWeight="bystuwt", treatment="treat", covariateNames=covariateNames),

  stabilized = balanceSummarizer(data=ELS.data.imputed, PSWeight="stabilizedWeightATE", effect="ATE",
                                 samplingWeight="bystuwt", treatment="treat", covariateNames=covariateNames),

  RF = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATERf", effect="ATE",
                         samplingWeight="bystuwt", treatment="treat", covariateNames=covariateNames),

  GBM = balanceSummarizer(data=ELS.data.imputed, PSWeight="weightATEGBM", effect="ATE",
                          samplingWeight="bystuwt", treatment="treat", covariateNames=covariateNames))



Table3.2 <- data.frame(round(Table3.2, digits = 4), index = rep(c("std.eff.sz", "varRatio"),5), method = rep(c("Logistic", "Truncated", "Stabilized", "RF", "GBM"), each=2))



# ----------
# Summary of absolute standardized effect sizes with weights for the ATE
# --> THe maximum standardized mean difference acrosss covariates is similar across methods.
# None of these methods procided covariate balance below 0.1 for all covariates.

Table3.2


