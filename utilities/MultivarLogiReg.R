# ------------------------------------------------------------------------------
# Multivariable Logistic Regression
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

MultivarLogiReg <- function(y_var, x_var, data, crit){
  require(broom)

  mult_lr <- data.frame()
  
  mod_char <- paste0(x_var, collapse = " + ")
  eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
  
  tmp <- tidy(mod)
  tmp2 <- tidy(confint(mod, levels = 0.95, log=TRUE))
  n <- nrow(tmp)
  var_n <- tmp$term
  coef <- round(tmp[tmp$term %in% var_n[1:n],"estimate"], digits = 4)
  std.error <- round(tmp[tmp$term %in% var_n[1:n],"std.error"], digits = 4)
  statistic <- round(tmp[tmp$term %in% var_n[1:n],"statistic"], digits = 4)
  odds.ratio <- round(exp(tmp[tmp$term %in% var_n[1:n],"estimate"]), digits = 4)
  odds.ratio_cil <- round(exp(tmp2[tmp2$.rownames %in% var_n[1:n], 2]), digits = 4)
  odds.ratio_ciu <- round(exp(tmp2[tmp2$.rownames %in% var_n[1:n], 3]), digits = 4)
  # odds.ratio_cil <- round(tmp2[tmp2$.rownames %in% var_n[1:n], 2], digits = 4)
  # odds.ratio_ciu <- round(tmp2[tmp2$.rownames %in% var_n[1:n], 3], digits = 4)
  p.value <- tmp[tmp$term %in% var_n[1:n],"p.value"]
  sig <- ifelse(p.value < crit[2], paste0("**  <", crit[2]), ifelse(p.value < crit[1], paste0("*  <", crit[1]), ""))
  
  mult_lr <- rbind(mult_lr, data.frame(var_n[1:n], coef, std.error, statistic, odds.ratio, odds.ratio_cil, odds.ratio_ciu, p.value, sig))
  
  colnames(mult_lr) <- c("VarName", "Coeff", "Std.Err", "Z", "OR-hat", "CI_0.025", "CI_0.975", "p.value", "sig")
  G.stat <- round(mod$null.deviance - mod$deviance, digits = 4)

  output <- list(mult_lr = mult_lr, G.stat = G.stat)
  return(output)
}


