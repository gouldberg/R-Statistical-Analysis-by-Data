# ------------------------------------------------------------------------------
# Comparison of Multivariable Logistic Regression models
# ------------------------------------------------------------------------------

ModCompLogiReg <- function(mod_list, mod_name, lrm = FALSE){
  require(rms);  require(Hmisc);  require(MLmetrics);  require(ResourceSelection);
  
  mod_comp <- data.frame()

  if(lrm == TRUE){  
    for(i in 1:length(mod_list)){
      mod <- mod_list[[i]]
      mod_n <- mod_name[i]
      mod <- modlrm_list[[i]]
      mod_n <- modlrm_name[i]
      print(paste0("processing -- ", mod_n))
      
      mod <- update(mod, x = TRUE, y = TRUE)
      
      null.deviance <- round(mod$deviance[1], digits = 2)
      deviance <- round(mod$deviance[2], digits = 2)
      G.stat <- round(mod$deviance[1] - mod$deviance[2], digits = 2)

      aic <- round(AIC(mod), digits = 2)
      bic <- round(BIC(mod), digits = 2)
      
      MaxDeriv <- round(unname(mod$stats[2]), digits = 2)
      ModelLR <- round(unname(mod$stats[3]), digits = 2)
      df <- round(unname(mod$stats[4]), digits = 0)
      LRT.pval <- round(unname(mod$stats[5]), digits = 5)
      C <- round(unname(mod$stats[6]), digits = 3)
      Dxy <- round(unname(mod$stats[7]), digits = 3)
      Gamma <- round(unname(mod$stats[8]), digits = 3)
      Tau_a <- round(unname(mod$stats[9]), digits = 3)
      R2 <- round(unname(mod$stats[10]), digits = 3)
      Brier <- round(unname(mod$stats[11]), digits = 3)
      g <- round(unname(mod$stats[12]), digits = 3)
      gr <- round(unname(mod$stats[13]), digits = 3)
      gp <- round(unname(mod$stats[14]), digits = 3)
      
      optimism <- unname(round(1 - (mod$stats["Model L.R."] - mod$stats["d.f."]) / mod$stats["Model L.R."], digits = 3))
  
      pp <- predict(mod, type = "fitted")
      ll <- as.numeric(mod$y)*1
      
      auc <- round(AUC(y_pred = pp, y_true = ll), digits = 3)
      gini <- round(Gini(y_pred = pp, y_true = ll), digits = 3)
      logloss <- round(LogLoss(y_pred = pp, y_true = ll), digits = 3)
      
      hoslem.pval <- try(round(hoslem.test(ll, pp, g=10)$p.val, digits = 5), silent=TRUE)
      if( class(hoslem.pval) == "try-error" ){
        print(paste0(mod_n, ":  hoslem.pval -- ", hoslem.pval))
        hoslem.pval <- NA
      }
      pearsonCor2 <- round(cor(ll, pp)^2, digits = 3)
  
      output <- data.frame(
        mod_name = mod_n,
        MaxDeriv = MaxDeriv,
        null.dev = null.deviance,
        dev = deviance,
        G.stat = G.stat,
        ModelLT = ModelLR,
        df = df,
        LRT.pval = LRT.pval,
        AIC = aic,
        BIC = bic,
        hoslem.pval = hoslem.pval,
        pearsonCor2 = pearsonCor2,
        optimism = optimism,
        C = C,
        Dxy = Dxy,
        Gamma = Gamma,
        Tau_a = Tau_a,
        R2 = R2,
        Brier = Brier,
        g = g,
        gr = gr,
        gp = gp,
        auc = auc,
        gini = gini,
        logloss = logloss
      )
      
      mod_comp <- rbind(mod_comp, output)
    }
  }
    
  if(lrm == FALSE){  
    for(i in 1:length(mod_list)){
      mod <- mod_list[[i]]
      mod_n <- mod_name[i]
      print(paste0("processing -- ", mod_n))
      
      null.deviance <- round(mod$null.deviance, digits = 2)
      deviance <- round(mod$deviance, digits = 2)
      G.stat <- round(mod$null.deviance - mod$deviance, digits = 2)

      ModelLR <- round(mod$null.deviance - mod$deviance, digits = 2)
      df <- mod$df.null - mod$df.residual
      LRT.pval <- round(1 - pchisq(ModelLR, df), digits = 5)
      
      aic <- round(AIC(mod), digits = 2)
      bic <- round(BIC(mod), digits = 2)
      
      optimism <- round(1 - (mod$null.deviance - mod$deviance - mod$df.null + mod$df.residual) / (mod$null.deviance - mod$deviance), digits = 3)
      
      pp <- predict(mod, newdata = mod$data, type = "response")
      eval(parse(text = paste0("ll <- as.numeric(mod$y) * 1")))

      auc <- round(AUC(y_pred = pp, y_true = ll), digits = 3)
      gini <- round(Gini(y_pred = pp, y_true = ll), digits = 3)
      logloss <- round(LogLoss(y_pred = pp, y_true = ll), digits = 3)

      hoslem.pval <- try(round(hoslem.test(ll, pp, g=10)$p.val, digits = 5), silent=TRUE)
      if( class(hoslem.pval) == "try-error" ){
        print(paste0(mod_n, ":  hoslem.pval -- ", hoslem.pval))
        hoslem.pval <- NA
      }
      pearsonCor2 <- round(cor(ll, pp)^2, digits = 3)
      
      output <- data.frame(
        mod_name = mod_n,
        null.dev = null.deviance,
        dev = deviance,
        G.stat = G.stat,
        ModelLT = ModelLR,
        df = df,
        LRT.pval = LRT.pval,
        AIC = aic,
        BIC = bic,
        hoslem.pval = hoslem.pval,
        pearsonCor2 = pearsonCor2,
        optimism = optimism,
        auc = auc,
        gini = gini,
        logloss = logloss
      )
      
      mod_comp <- rbind(mod_comp, output)
    }
  }
    
  return(mod_comp)
}


