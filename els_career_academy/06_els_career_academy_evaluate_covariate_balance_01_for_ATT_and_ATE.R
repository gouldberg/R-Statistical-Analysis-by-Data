setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Evaluate covariate balance for ATT
# ------------------------------------------------------------------------------

# create final weights for the base year
# the final weigth is the product of the propensity score weight and the sampling weight

ELS.data.imputed$finalWeightBY <- with(ELS.data.imputed, bystuwt * weightATT)



# ----------
# bal.stat computes the weighted means for continuous covariates,
# weighted proportions for categorical covariates, 
# standard deviations, and standardized mean
# or proportion differences between treated and untreated groups for each covariate.
# With the val.stat function, standardized mean or proportion differences are computed using the standard deviation of the treated group rather than the
# pooled standard deviation.

library(twang)

balanceTable <- bal.stat(ELS.data.imputed, vars = covariateNames, treat.var = "treat", 
                         w.all = ELS.data.imputed$finalWeightBY, get.ks = F, sampw = ELS.data.imputed$bystuwt, estimand = "ATT", multinom = F)



# Table with results of balance evaluation. The columns are:
# tx.mn - The mean of the treatment group
# tx.sd	- The standard deviation of the treatment group
# ct.mn - The mean of the control group
# ct.sd	- The standard deviation of the control group
# std.eff.sz	- The standardized effect size, (tx.mn-ct.mn)/tx.sd. 
# stat	- the t-statistic for numeric variables and the chi-square statistic for categorical variables
# p	- the p-value for the test associated with stat

balanceTable <- balanceTable$results 


head(balanceTable)




# ------------------------------------------------------------------------------
# Evaluate covariate balance for ATE
# ------------------------------------------------------------------------------

balanceTable2 <- bal.stat(ELS.data.imputed, vars = covariateNames, treat.var = "treat", 
                         w.all = ELS.data.imputed$finalWeightBY, get.ks = F, sampw = ELS.data.imputed$bystuwt, estimand = "ATE", multinom = F)


balanceTable2 <- balanceTable2$results 



head(balanceTable2)



# -->
# Note that the absolute value of std.eff.sz
# is smaller for ATE (balanceTable2 < balanceTable1)
