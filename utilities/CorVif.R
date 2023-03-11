# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------
# Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  # invisible(result)
}
#END VIF FUNCTIONS


corvif <- function(data, var) {
  tmp <- as.data.frame(data)
  
  #vif part
  form    <- formula(paste0("fooy ~ ", paste0(var, collapse=" + ")))
  tmp   <- data.frame(fooy=1 + rnorm(nrow(tmp)), tmp)
  lm_mod  <- lm(form, tmp)
  
  gvif <- myvif(lm_mod)
  cat("\n\nVariance inflation factors\n\n")
  print(gvif)
  return(gvif)
}

# Select variables by VIF
select_var_bycorvif <- function(data, var, crit) {
  tmp <- as.data.frame(data)
  removed <- ""
  
  for(i in 1:length(var)){
    form    <- formula(paste0("fooy ~ ", paste0(var, collapse=" + ")))
    tmp   <- data.frame(fooy=1 + rnorm(nrow(tmp)), tmp)
    lm_mod  <- lm(form, tmp)
    
    gvif <- myvif(lm_mod)
    cat("\nVariance inflation factors\n")

    maxgif <- max(gvif)
    if(maxgif < crit)  break

    var_maxgif <- var[which(gvif == max(gvif))]
    var <- setdiff(var, var_maxgif)
    tmp <- tmp[,var]
    removed <- paste0(removed, var_maxgif, collapse = "  ")
    cat(paste0("\nRemoved ", i, " variables:   ", removed))
  }

  var_slc <- rownames(gvif)
  cat(paste0("\nSelected ", length(var_slc), " variables: ", paste0(var_slc, collapse= "  "), "\n"))
  cat("\nPearson Correlation Matrix\n")
  print(round(cor(data[,var_slc], method="pearson"), digits = 3))
  cat("\nGVIF\n")
  print(round(gvif, digits = 3))
  return(gvif)
}


