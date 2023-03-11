setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Evaluate coariate balance for the ATE
# ------------------------------------------------------------------------------

library(twang)

# create a numeric treatment indicator, required by twang
data.stratification$treat2 <- ifelse(data.stratification$treat == "Treated", 1, 0) 


# balance evaluation 
balanceTableMMWS <- bal.stat(
  data = data.stratification, 
  estimand = "ATE",  w.all = data.stratification$mmwsATEFinal, get.ks = F, vars = covariateNames, 
  treat.var = "treat2", sampw = data.stratification$FINALWGT, multinom = F)

balanceTableMMWS <- balanceTableMMWS$results[,1:5]

balanceTableMMWS$varRatio <- with(balanceTableMMWS, tx.sd^2 / ct.sd^2) 

unbalanced.covariatesATE = list(
  std.eff.sz = rownames(balanceTableMMWS)[abs(balanceTableMMWS$std.eff.sz) > 0.1],
  varRatio = rownames(balanceTableMMWS)[balanceTableMMWS$varRatio < 0.8 | balanceTableMMWS$varRatio > 1.2 ])


# examine balance statistics
summary(balanceTableMMWS)
summaryTableMMWS <- c()
for (var in 1:ncol(balanceTableMMWS)) {
  summaryTableMMWS = rbind(summaryTableMMWS, summary(balanceTableMMWS[,var])) }

rownames(summaryTableMMWS) = colnames(balanceTableMMWS)

#write.csv(summaryTableMMWS[,c(1,4,6)], file="summary_balance_MMWSATE.csv")



# ------------------------------------------------------------------------------
# Evaluate coariate balance for the ATT
# ------------------------------------------------------------------------------
library(twang)

# create a numeric treatment indicator, required by twang
data.stratification$treat2 <- ifelse(data.stratification$treat == "Treated", 1, 0) 


# balance evaluation 
balanceTableMMWSATT <- bal.stat(
  data = data.stratification, 
  estimand = "ATT",  w.all = data.stratification$mmwsATTFinal, get.ks = F, vars = covariateNames, 
  treat.var = "treat2", sampw = data.stratification$FINALWGT, multinom = F)

balanceTableMMWSATT <- balanceTableMMWSATT$results[,1:5]

balanceTableMMWSATT$varRatio <- with(balanceTableMMWSATT, tx.sd^2 / ct.sd^2) 


# examine balance statistics
summary(balanceTableMMWSATT)

summaryTableMMWSATT = c()

for (var in 1:ncol(balanceTableMMWSATT)) {
  summaryTableMMWSATT = rbind(summaryTableMMWSATT, summary(balanceTableMMWSATT[,var])) }

rownames(summaryTableMMWSATT) = colnames(balanceTableMMWSATT)


# write.csv(summaryTableMMWSATT[,c(1,4,6)], file = "summary_balance_MMWSATT.csv")

unbalanced.covariatesATT <- list(
  std.eff.sz = rownames(balanceTableMMWSATT)[abs(balanceTableMMWSATT$std.eff.sz) > 0.1],
  varRatio = rownames(balanceTableMMWSATT)[balanceTableMMWSATT$varRatio < 0.8 | balanceTableMMWSATT$varRatio > 1.2 ])
