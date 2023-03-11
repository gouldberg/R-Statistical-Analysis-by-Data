# ------------------------------------------------------------------------------
# Comparison of Various Poisson Count Regression Models
# ------------------------------------------------------------------------------

ModCompPoisReg <- function(mod_list, mod_name){
  require(MLmetrics);  require(ResourceSelection);
  require(broom);
  
  mod_comp <- data.frame()

  for(i in 1:length(mod_list)){
    mod <- mod_list[[i]]
    mod_n <- mod_name[i]
    print(paste0("processing -- ", mod_n))
    
    if(is.null(mod$deviance)){
      null.deviance <- "NA";  deviance <- "NA";  G.stat <- "NA";  ModelLR <- "NA";  LRT.pval <- "NA";
      df <- mod$df.null - mod$df.residual
    } else {
      null.deviance <- round(mod$null.deviance, digits = 2)
      deviance <- round(mod$deviance, digits = 2)
      G.stat <- round(mod$null.deviance - mod$deviance, digits = 2)
      ModelLR <- round(mod$null.deviance - mod$deviance, digits = 2)
      df <- mod$df.null - mod$df.residual
      LRT.pval <- round(1 - pchisq(ModelLR, df), digits = 5)
    }

    aic <- round(AIC(mod), digits = 2)
    bic <- round(BIC(mod), digits = 2)
    
    # optimism <- round(1 - (mod$null.deviance - mod$deviance - mod$df.null + mod$df.residual) / (mod$null.deviance - mod$deviance), digits = 3)
    
    try(newdata <- mod$data, silent = TRUE)
    if(is.null(newdata)){
      try(newdata <- mod$model, silent = TRUE)
    }
      
    pp <- predict(mod, newdata = newdata, type = "response")
    eval(parse(text = paste0("ll <- as.numeric(mod$y) * 1")))

    pearsonCor <- round(cor(ll, pp, method="pearson"), digits = 3)
    spearmanRankCor <- round(cor(ll, pp, method="spearman"), digits = 3)

    rmse <- round(RMSE(y_pred = pp, y_true = ll), digits = 3)
    mae <- round(MAE(y_pred = pp, y_true = ll), digits = 3)
    
    tmp <- tidy(glm(ll ~ pp, family=gaussian, data=newdata))
    intercept <- round(tmp[1,"estimate"], digits = 3)
    slope <- round(tmp[2,"estimate"], digits = 3)
    
    output <- data.frame(
      mod_name = mod_n,
      pearsonCor = pearsonCor,
      spearmanRankCor = spearmanRankCor,
      Intercept.predvstrue = intercept,
      Slope.predvstrue = slope,
      null.dev = null.deviance,
      deviance = deviance,
      G.stat = G.stat,
      ModelLR = ModelLR,
      df = df,
      LRT.pval = LRT.pval,
      AIC = aic,
      BIC = bic,
      RMSE = rmse,
      MAE = mae
    )
    
    mod_comp <- rbind(mod_comp, output)
  }

  return(mod_comp)
}


