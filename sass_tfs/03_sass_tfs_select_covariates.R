# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\sass_tfs")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SASS and TFS
# ------------------------------------------------------------------------------


load("SASS_TFS_data_imputed.Rdata")


str(imputedData)


names(imputedData)


dim(imputedData)



# ------------------------------------------------------------------------------
# Evaluate covariate balance as baseline for all variables
# treatment dose ~ each covariate
# ------------------------------------------------------------------------------


covariateNames0 <- colnames(imputedData)


covariateNames0 <- setdiff(covariateNames0, c("Treat", "leftTeaching"))



balanceTable_0 <- data.frame()


for (var in 1:length(covariateNames0)) {
  
  formula0 <- paste("Treat ~ ", covariateNames0[var], sep = "")
  
  
  # ----------
  # (-1) removes intercept
  # for dummy-coded categorical covariates, the largest coefficient of the dummy codes is selected
  
  maxEff <- max(abs(coef(lm(formula0, imputedData))[-(1)]))
  
  balanceTable_0 <- rbind(balanceTable_0, c(var, maxEff))
}


names(balanceTable_0) <- c("variable", "coef_to_treat") 

balanceTable_0$variable <- covariateNames0



# ----------
# standardize coefficients with respect to sd of outcome

balanceTable_0$coef_to_treat <- balanceTable_0$coef_to_treat / sd(imputedData$Treat)


balanceTable_0$coef_to_treat <- round(balanceTable_0$coef_to_treat, 4)


balanceTable_to_treat




# ------------------------------------------------------------------------------
# Evaluate linear regression coefficients
# outcome ~ each covariate
# ------------------------------------------------------------------------------


covariateNames0 <- colnames(data)


covariateNames0 <- setdiff(covariateNames0, c("logLoginsPerExaminee", "loginsPerExaminee", "meanScale2012"))



balanceTable_02 <- data.frame()


for (var in 1:length(covariateNames0)) {
  
  formula02 <- paste("meanScale2012 ~ ", covariateNames0[var], sep = "")
  
  
  # ----------
  # (-1) removes intercept
  # for dummy-coded categorical covariates, the largest coefficient of the dummy codes is selected
  
  maxEff <- max(abs(coef(lm(formula02, data))[-(1)]))
  
  balanceTable_02 <- rbind(balanceTable_02, c(var, maxEff))
}


names(balanceTable_02) <- c("variable", "coef_to_outcome") 

balanceTable_02$variable <- covariateNames0



# ----------
# standardize coefficients with respect to sd of outcome

balanceTable_02$coef_to_outcome <- balanceTable_02$coef_to_outcome / sd(data$meanScale2012)


balanceTable_02$coef_to_outcome <- round(balanceTable_02$coef_to_outcome, 4)


balanceTable_02




# ------------------------------------------------------------------------------
# marge both
# ------------------------------------------------------------------------------


balanceTable_init <- balanceTable_0 %>% left_join(., balanceTable_02, by = "variable")


balanceTable_init




# ------------------------------------------------------------------------------
# Select one of standardized coefficients (to_treat, to_outcome) > 0.1
# (just for reference, 0.1 does not apply generalized propensity score case)
# ------------------------------------------------------------------------------


balanceTable_init %>% filter(coef_to_treat > 0.1 | coef_to_outcome > 0.1)


covariate_selected <- balanceTable_init %>% filter(coef_to_treat > 0.1 | coef_to_outcome > 0.1) %>% dplyr::select(variable) %>% pull()


covariate_selected




# ------------------------------------------------------------------------------
# Check the correlation among covariate_selected
# ------------------------------------------------------------------------------


col_type <- sapply(1:length(covariate_selected), function(x) class(data[,covariate_selected[x]]))


( col_obj <- names(data[,covariate_selected])[col_type %in% c("integer", "numeric")] )



( cor_among_voc <- data[,col_obj] %>% cor(., method = "spearman") %>% as.table() %>% as.data.frame() %>%
    filter(Var1 != Var2) %>% filter(abs(Freq) > 0.4) )




# ------------------------------------------------------------------------------
# Define covariates
# ------------------------------------------------------------------------------

covariate_selected


covariateNames <- c("Charter", #dummy indicator of charter school
                    "Magnet.", #dummy indicator of magnet school
                    "Title.I.School.", #dummy indicator of title 1 school
                    "locationRural",  #rural location
                    "locationSize", #size of location (large, midsize, small)
                    "Students.", #total number of students in 2012
                    "SeniorHigh" ,    #dummy indicator of whether it is a senior high school (the reference group is high school                          
                    "numOfStud2014", #number of test takers in 2014 
                    "meanScale2012", #mean scaled scores in 2012
                    "lev1Perc2012",#percent achieving level 1 in 2012
                    "lev5Perc2012", #percent achieving level 5 in 2012
                    "perc.free.lunch", #percent free lunch in 2012
                    "perc.reduced.lunch") #percent reduced lunch in 2012


setdiff(covariate_selected, covariateNames)

setdiff(covariateNames, covariate_selected)





