# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)


names(data)




# ----------
# Standardize continuous covariates

data$numOfStud2014 <- scale(data$numOfStud2014)

data$meanScale2012 <- scale(data$meanScale2012)

data$lev1Perc2012 <- scale(data$lev1Perc2012)

data$lev5Perc2012 <- scale(data$lev5Perc2012)

data$perc.free.lunch <- scale(data$perc.free.lunch)

data$perc.reduced.lunch <- scale(data$perc.reduced.lunch)



# ----------
# convert binary variables to factors

data$SeniorHigh <- factor(data$SeniorHigh)

data$middleHigh <- factor(data$middleHigh)




# ----------
# breakdown locale into size and type variables

data$locationSize <- with(data, ifelse(Locale.=="City: Large" |
                                         Locale.=="Suburb: Large", "Large", ifelse( Locale.=="City: Midsize"|
                                                                                      Locale.=="Suburb: Midsize", "Midsize", "Small")))  
data$locationSize <- factor(data$locationSize)


data$locationRural <- with(data, ifelse(Locale.=="Rural: Distant" |
                                          Locale.=="Rural: Fringe" |
                                          Locale.=="Rural: Remote", "Rural", "Urban"))  

data$locationRural <- factor(data$locationRural)




# ------------------------------------------------------------------------------
# Assess covariate balance as baseline and after stratification
# ------------------------------------------------------------------------------


balanceTable <- data.frame()


for (var in 1:length(covariateNames)) {
  
  formula_base <- paste("logLoginsPerExaminee ~ ", covariateNames[var], sep = "")
  formula_balance <- paste("logLoginsPerExaminee ~ strataGPS + ", covariateNames[var], sep = "")
  
  
  # ----------
  # regress dose on covariate without weights
  # -(1:5):  remove intercept and strataGPS2 - 5
  # for dummy-coded categorical covariates, the largest coefficient of the dummy codes is selected
  
  maxEff_base <- max(abs(coef(lm(formula_base, data))[-(1)]))
  maxEff_balance <- max(abs(coef(lm(formula_balance, data))[-(1:5)]))
  
  balanceTable <- rbind(balanceTable, c(var, maxEff_base, maxEff_balance))
}


names(balanceTable) <- c("variable", "coef_base", "coef_balance") 

balanceTable$variable <- covariateNames




# ----------
# standardize coefficients with respect to sd of outcome

balanceTable$coef_base <- round(balanceTable$coef_base / sd(data$logLoginsPerExaminee), 4)

balanceTable$coef_balance <- round(balanceTable$coef_balance / sd(data$logLoginsPerExaminee), 4)


balanceTable




# ----------
balanceTable %>% filter(coef_balance > 0.1)




# -->
# Covariate balance is considered adequate if the standardized regression coefficient is lower than 0.1,
# but guidelines for covariate balance evaluation for propensity score analysis with treatment does
# are not yet well established.

# Although the covariate balance improved substantially compared with the baseline,
# three covariates (charter school, magnet school, and location size) did not achieve the desired level of balance.



