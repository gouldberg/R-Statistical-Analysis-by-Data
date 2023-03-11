# ------------------------------------------------------------------------------
# Main effects + interaction term added individually
# Logistic Regression
#
# INPUT
#  - y_var:  response / outcome variable name
#  - x_var:  vector of explanatory variable names
#  - data:  dataframe to be input to modeling
#  - crit:  vector of 2 values such as c(0.05, 0.01) 
#
# OUTPUT:  dataframe
#  - Explanatory Variable Name (Covariate)
#  - Estimated coefficient
#  - Standard error of the estimated coefficient
#  - Estimated odds ratio = exp(Estimated coeffs)
#  - 95% confidence interval of odds ratio
#  - p.value:  HO: beta1 = 0
#  - significance at specified crit level
#  - G statistic = D(variable without variable) - D(Variable with variable)
# 
# OUTPUT:  G statistic = D(variable without variable) - D(Variable with variable)
#
# NOTE: Does not return glm mod object !!!
#
# ------------------------------------------------------------------------------

MainInterLogiReg <- function(y_var, x_main_var, x_add_inter_var, data, crit){
  require(broom)
  require(dplyr)

  x_inter_var <- combinat::combn(x_add_inter_var, m = 2) %>% t() %>% as.data.frame() %>% mutate(x_inter_var = paste0(V1, ":", V2)) %>% dplyr::select(x_inter_var) %>% unlist() %>% unname()
  
  main_inter_lr <- data.frame()
  mod_char_main <- paste0(x_main_var, collapse = " + ")
  
  eval(parse(text = paste0("mod_main <- glm(", y_var, " ~ ", mod_char_main, ", data = data, family = binomial)")))
  n_main <- nrow(tidy(mod_main))
  var_main_n <- tidy(mod_main)$term
  G.stat_main <- mod_main$null.deviance - mod_main$deviance
  
  for(i in 1:length(x_inter_var)){
    cat(paste0("processing -- ", x_inter_var[i], "\n"))
    
    mod_char <- paste0(mod_char_main, " + ", x_inter_var[i])
    eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
    
    tmp <- tidy(mod)
    tmp2 <- tidy(confint(mod, levels = 0.95, log=TRUE))
    # tmp2 <- tidy(confint(mod, levels = 0.95, log=FALSE))
    
    n <- nrow(tmp) - n_main
    var_n <- setdiff(tmp$term, var_main_n)
    coef <- round(tmp[tmp$term %in% var_n[1:n],"estimate"], digits = 4)
    std.error <- round(tmp[tmp$term %in% var_n[1:n],"std.error"], digits = 4)
    odds.ratio <- round(exp(tmp[tmp$term %in% var_n[1:n],"estimate"]), digits = 4)
    odds.ratio_cil <- round(exp(tmp2[tmp2$.rownames %in% var_n[1:n], 2]), digits = 4)
    odds.ratio_ciu <- round(exp(tmp2[tmp2$.rownames %in% var_n[1:n], 3]), digits = 4)
    # odds.ratio_cil <- round(tmp2[tmp2$.rownames %in% var_n[1:n], 2], digits = 4)
    # odds.ratio_ciu <- round(tmp2[tmp2$.rownames %in% var_n[1:n], 3], digits = 4)
    aic <- round(AIC(mod), digits = 2)
    bic <- round(BIC(mod), digits = 2)
    G.stat <- round(mod$null.deviance - mod$deviance - G.stat_main, digits = 4)
    p.value <- round(tmp[tmp$term %in% var_n[1:n],"p.value"], digits = 5)
    sig <- ifelse(p.value < crit[3], paste0("***  <", crit[3]), ifelse(p.value < crit[2], paste0("**  <", crit[2]), ifelse(p.value < crit[1], paste0("*  <", crit[1]), "")))
    
    main_inter_lr <- rbind(main_inter_lr, data.frame(var_n[1:n], coef, std.error, odds.ratio, odds.ratio_cil, odds.ratio_ciu, aic, bic, G.stat, p.value, sig))
  }
  
  colnames(main_inter_lr) <- c("InteractionTerm", "Coeff", "Std.Err", "OR-hat", "CI_0.025", "CI_0.975", "AIC", "BIC", "G.stat", "p.value", "sig")
  return(main_inter_lr)
}


