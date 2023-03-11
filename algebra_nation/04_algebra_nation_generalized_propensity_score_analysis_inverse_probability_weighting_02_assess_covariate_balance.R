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
# Evaluate covariate balance:  applying IPW weight
# ------------------------------------------------------------------------------


# Apply IPW weight here !!

designIPW <- svydesign(ids = ~ 1, weights = ~ IPW, data = data)


balanceTableIPW <- data.frame()


for (var in 1:length(covariateNames)) {
  
  balanceFormula <- paste("logLoginsPerExaminee ~ ", covariateNames[var], sep = "")
  
  maxEffBaseline <- max(abs(coef(svyglm(balanceFormula, designAN))[-1]))
  
  maxEffIPW <- max(abs(coef(svyglm(balanceFormula, designIPW))[-1]))  
  
  balanceTableIPW <- rbind(balanceTableIPW, c(var, maxEffBaseline, maxEffIPW))
  
}



names(balanceTableIPW) <- c("variable", "coefBaseline", "coefIPW") 

balanceTableIPW$variable <- covariateNames

balanceTableIPW$coefBaseline <- round(balanceTableIPW$coefBaseline / sqrt(coef(svyvar( ~ logLoginsPerExaminee, designAN))), 4)

balanceTableIPW$coefIPW <- round(balanceTableIPW$coefIPW / sqrt(coef(svyvar(~ logLoginsPerExaminee, designIPW))), 4)




# for reference:  balance after GPS stratification

balanceTableIPW$coef_balance <- balanceTable$coef_balance



# ----------
balanceTableIPW



balanceTableIPW %>% filter(coef_balance > 0.1)



# -->
# covariate balance improved with the IPW compared with both the baseline and the GPS strata

# compared to balance after GPS stratification, Magnet and locationSize is also balanced

# All covariates have standardized coefficients lower than 0.1, except for the variable indicating that
# the school was a charter school.

# This variable is included in the outcome model to provide additional bias reduction.



