
packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Absolute Standardized Mean Difference
# ------------------------------------------------------------------------------


data <- as.data.frame(biased_data)



# ----------

covariateNames0 <- colnames(data)

covariateNames0 <- setdiff(covariateNames0, c("spend", "treatment"))

col_type <- sapply(1:length(covariateNames0), function(x) class(data[,covariateNames0[x]]))

col_num <- covariateNames0[col_type %in% c("numeric", "integer")]

col_fac <- covariateNames0[col_type %in% c("character", "factor")]




# ----------
tmp0 <- data %>% filter(treatment == 0)

tmp1 <- data %>% filter(treatment == 1)


balanceTable_asm <- data.frame()


for(i in 1:length(col_num)){
  obj_col <- col_num[i]
  
  obj <- round(abs(mean(tmp1[,obj_col]) - mean(tmp0[,obj_col])) / sd(data[,obj_col]), 4)
  
  balanceTable_asm <- rbind(balanceTable_asm, c(i, obj))
}


names(balanceTable_asm) <- c("Variable", "ASM") 

balanceTable_asm$Variable <- col_num




# ----------
balanceTable_asm2 <- data.frame()


for(i in 1:length(col_fac)){
  obj_col <- col_fac[i]
  
  tmp0_p <- prop.table(table(tmp0[,obj_col])) %>% data.frame()
  tmp1_p <- prop.table(table(tmp1[,obj_col])) %>% data.frame()
  names(tmp0_p) <- c("Variable", "prop0")
  names(tmp1_p) <- c("Variable", "prop1")
  
  tmp_df <- data.frame(Variable = unique(c(as.character(tmp0_p$Variable), as.character(tmp1_p$Variable))))
  tmp_df <- tmp_df %>% left_join(., tmp0_p, by = "Variable") %>% left_join(., tmp1_p, by = "Variable")
  
  tmp_df$prop0[is.na(tmp_df$prop0)] <- 0
  tmp_df$prop1[is.na(tmp_df$prop1)] <- 0
  
  tmp_df <- tmp_df %>% mutate(ASM = round(abs(prop1 - prop0), 3)) %>% dplyr::select(Variable, ASM)
  
  balanceTable_asm2 <- rbind(balanceTable_asm2, tmp_df)
}



# ----------
balanceTable_asm <- rbind(balanceTable_asm, balanceTable_asm2)


balanceTable_asm


balanceTable_asm %>% filter(ASM > 0.1)




# ------------------------------------------------------------------------------
# Evaluate covariate balance as baseline for all variables
# treatment ~ each covariate
# ------------------------------------------------------------------------------


covariateNames0 <- colnames(data)

covariateNames0 <- setdiff(covariateNames0, c("spend", "treatment"))



balanceTable_0 <- data.frame()


for (var in 1:length(covariateNames0)) {
  
  formula0 <- paste("treatment ~ ", covariateNames0[var], sep = "")
  
  
  # ----------
  # (-1) removes intercept
  # for dummy-coded categorical covariates, the largest coefficient of the dummy codes is selected
  
  maxEff <- max(abs(coef(lm(formula0, data))[-(1)]))
  
  balanceTable_0 <- rbind(balanceTable_0, c(var, maxEff))
}


names(balanceTable_0) <- c("variable", "coef_to_treat") 

balanceTable_0$variable <- covariateNames0



# ----------
# standardize coefficients with respect to sd of outcome

balanceTable_0$coef_to_treat <- balanceTable_0$coef_to_treat / sd(data$treatment)


balanceTable_0$coef_to_treat <- round(balanceTable_0$coef_to_treat, 4)


balanceTable_0




# ------------------------------------------------------------------------------
# Evaluate linear regression coefficients
# outcome ~ each covariate
# ------------------------------------------------------------------------------


covariateNames0 <- colnames(data)

covariateNames0 <- setdiff(covariateNames0, c("spend", "treatment"))



balanceTable_02 <- data.frame()


for (var in 1:length(covariateNames0)) {
  
  formula02 <- paste("spend ~ ", covariateNames0[var], sep = "")
  
  
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

balanceTable_02$coef_to_outcome <- balanceTable_02$coef_to_outcome / sd(data$spend)


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




