# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------
BoxCoxTrans_byvar <- function(data, var){
  require(caret)
  
  tmp <- as.data.frame(data)
  output <- data.frame()
  
  for(i in 1:length(var)){
    cat(paste0("\nProcessing: ", var[i]))
    bc2 <- BoxCoxTrans(tmp[,var[i]], na.rm=TRUE)

    var_n <- var[i]
    lambda <- bc2$lambda
    min_v <- round(bc2$summary[1], digits = 3) %>% as.vector()
    q1_v <- round(bc2$summary[2], digits = 3) %>% as.vector()
    med_v <- round(bc2$summary[3], digits = 3) %>% as.vector()
    mean_v <- round(bc2$summary[4], digits = 3) %>% as.vector()
    q3_v <- round(bc2$summary[5], digits = 3) %>% as.vector()
    max_v <- round(bc2$summary[6], digits = 3) %>% as.vector()
    ratio_v <- round(bc2$ratio, digits = 3) %>% as.vector()
    skewness_v <- round(bc2$skewness, digits = 3) %>% as.vector()
    
    output0 <- data.frame(
      var = var_n,
      lambda = lambda,
      ratio = ratio_v,
      skewness = skewness_v,
      min = min_v,
      Q1 = q1_v,
      median = med_v,
      mean = mean_v,
      Q3 = q3_v,
      max = max_v
    )    
      
    output <- rbind(output, output0)
  }
  
  cat(paste0("\nFeasibility check for Box-Cox Transformation\n"))
  #print(output)
  return(output)
}

