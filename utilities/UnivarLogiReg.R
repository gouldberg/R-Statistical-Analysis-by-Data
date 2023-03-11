# ------------------------------------------------------------------------------
# Univariable Logistic Regression
#
# INPUT
#  - y_var:  response / outcome variable name
#  - x_var:  vector of explanatory variable names
#  - data:  dataframe to be input to modeling
#  - crit:  vector of 3 values such as c(0.25, 0.05, 0.01) 
#
# OUTPUT:  dataframe
#  - Explanatory Variable Name (Covariate)
#  - Estimated coefficient
#  - Standard error of the estimated coefficient
#  - Estimated odds ratio = exp(Estimated coeffs)
#  - 95% confidence interval of odds ratio
#  - G statistic = D(variable without variable) - D(Variable with variable)
#  - p.value:  HO: beta1 = 0
#  - significance at specified crit level
#
# OUTPUT:  plotted chart by popbio::logi.hist.plot
#  - available if the explanatory variable is integer or numeric class
# ------------------------------------------------------------------------------

UnivarLogiReg <- function(y_var, x_var, data, crit){
  require(broom)
  require(popbio)
  
  uni_lr <- data.frame()
  
  for(i in 1:length(x_var)){
    cat(paste0("processing -- ", x_var[i], "\n"))
    
    eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", x_var[i], ", data = data, family = binomial)")))
    
    tmp <- tidy(mod)
    tmp2 <- tidy(confint(mod, levels = 0.95, log=TRUE))
    # tmp2 <- tidy(confint(mod, levels = 0.95, log=FALSE))

    n <- nrow(tmp)
    var_n <- tmp$term
    coef <- round(tmp[tmp$term %in% var_n[2:n],"estimate"], digits = 4)
    std.error <- round(tmp[tmp$term %in% var_n[2:n],"std.error"], digits = 4)
    odds.ratio <- round(exp(tmp[tmp$term %in% var_n[2:n],"estimate"]), digits = 4)
    odds.ratio_cil <- round(exp(tmp2[tmp2$.rownames %in% var_n[2:n], 2]), digits = 4)
    odds.ratio_ciu <- round(exp(tmp2[tmp2$.rownames %in% var_n[2:n], 3]), digits = 4)
    # odds.ratio_cil <- round(tmp2[tmp2$.rownames %in% var_n[2:n], 2], digits = 4)
    # odds.ratio_ciu <- round(tmp2[tmp2$.rownames %in% var_n[2:n], 3], digits = 4)
    G.stat <- round(mod$null.deviance - mod$deviance, digits = 4)
    p.value <- tmp[tmp$term %in% var_n[2:n],"p.value"]
    sig <- ifelse(p.value < crit[3], paste0("***  <", crit[3]), ifelse(p.value < crit[2], paste0("**  <", crit[2]), ifelse(p.value < crit[1], paste0("*  <", crit[1]), "")))

    uni_lr <- rbind(uni_lr, data.frame(var_n[2:n], coef, std.error, odds.ratio, odds.ratio_cil, odds.ratio_ciu, G.stat, p.value, sig))

    # plotting by popbio:logi.hist.plot if the variable is integer or numeric
    if(class(data[,x_var[i]]) %in% c("integer", "numeric")){
      par(mfrow=c(1,1))
      eval(parse(text = paste0(
        "with(data, 
       logi.hist.plot(independ = ", x_var[i], ", depend = ", y_var, ", 
                              logi.mod = 1,
                              type='hist', counts=TRUE, 
                              boxp = TRUE, rug = TRUE,
                              ylabel = 'Probability (", y_var, ")', xlabel = '", x_var[i], "',
                              col.hist = 'lightblue'))")))
    }
  }

  colnames(uni_lr) <- c("VarName", "Coeff", "Std.Err", "OR-hat", "CI_0.025", "CI_0.975", "G.stat", "p.value", "sig")
  return(uni_lr)
}


