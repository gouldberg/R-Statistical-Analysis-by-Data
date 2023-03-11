# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

# Quassipoisson:  Dropping variables by drop1(., test="Chi")
drop1_byvar_qpois <- function(data, x_var, y_var, crit) {
  require(broom)
  
  tmp <- as.data.frame(data)
  var <- x_var
  dropped <- ""
  varn <- length(x_var)
  form <- formula(paste0(y_var, " ~ ", paste0(var, collapse=" + ")))
  mod <- glm(form, data = tmp, family=quasipoisson)
  
  for(i in 1:length(x_var)){

    drop_result <- drop1(mod, test="Chi")
    max_pr <- max(drop_result[2:(varn+1),"Pr(>Chi)"])
    var_maxpr <- rownames(drop_result)[which(drop_result[,"Pr(>Chi)"] == max_pr)]

    var <- setdiff(var, var_maxpr)
    form <- formula(paste0(y_var, " ~ ", paste0(var, collapse=" + ")))
    mod <- glm(form, data = tmp, family=quasipoisson)
    mod_result <- tidy(mod)
    max_pr_mod <- max(mod_result[,"p.value"])
    varn <- length(var)
    dropped <- paste0(c(dropped, var_maxpr), collapse = "  ")
    cat(paste0("\ndropped ", i, " variables:   ", dropped))
    
    if(max_pr_mod < crit)  break
  }

  var_slc <- var
  form <- formula(paste0(y_var, " ~ ", paste0(var_slc, collapse=" + ")))
  mod <- glm(form, data = tmp, family=quasipoisson)
  cat(paste0("\nSelected ", length(var_slc), " variables: ", paste0(var_slc, collapse= "  "), "\n"))
  print(summary(mod))
  return(mod)
}


# Negative Binomial:  Dropping variables by drop1(., test="Chi")
drop1_byvar_nbin <- function(data, x_var, y_var, crit) {
  require(broom)
  
  tmp <- as.data.frame(data)
  var <- x_var
  dropped <- ""
  varn <- length(x_var)
  form <- formula(paste0(y_var, " ~ ", paste0(var, collapse=" + ")))
  mod <- glm.nb(form, data = tmp)
  theta <- mod$theta
  mod <- glm(form, data = tmp, family=negative.binomial(theta))
  
  for(i in 1:length(x_var)){
    
    drop_result <- drop1(mod, test="Chi")
    max_pr <- max(drop_result[2:(varn+1),"Pr(>Chi)"])
    var_maxpr <- rownames(drop_result)[which(drop_result[,"Pr(>Chi)"] == max_pr)]
    
    var <- setdiff(var, var_maxpr)
    form <- formula(paste0(y_var, " ~ ", paste0(var, collapse=" + ")))
    mod <- glm.nb(form, data = tmp)
    theta <- mod$theta
    mod <- glm(form, data = tmp, family=negative.binomial(theta))
    mod_result <- tidy(mod)
    max_pr_mod <- max(mod_result[,"p.value"])
    varn <- length(var)
    dropped <- paste0(c(dropped, var_maxpr), collapse = "  ")
    cat(paste0("\ndropped ", i, " variables:   ", dropped))
    
    if(max_pr_mod < crit)  break
  }
  
  var_slc <- var
  form <- formula(paste0(y_var, " ~ ", paste0(var_slc, collapse=" + ")))
  mod <- glm.nb(form, data = tmp)
  theta <- mod$theta
  mod <- glm(form, data = tmp, family=negative.binomial(theta))
  cat(paste0("\nSelected ", length(var_slc), " variables: ", paste0(var_slc, collapse= "  "), "\n"))
  print(summary(mod))
  return(mod)
}



